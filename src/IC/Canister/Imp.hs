{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

{-|
The canister interface, presented imperatively (or impurely), i.e. without rollback
-}
module IC.Canister.Imp
 ( CanisterEntryPoint
 , ImpState(..)
 , rawHeartbeat
 , rawGlobalTimer
 , rawInstantiate
 , rawInitialize
 , rawQuery
 , rawUpdate
 , rawCallback
 , rawCleanup
 , rawPreUpgrade
 , rawPostUpgrade
 , rawInspectMessage
 )
where

import qualified Data.Text as T
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSU
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Except
import Data.Bits
import Data.STRef
import Data.Maybe
import Data.Int -- TODO: Should be Word32 in most cases
import Data.Word
import Data.Functor
import Numeric.Natural

import IC.Types
import IC.Wasm.Imports
import IC.Canister.StableMemory as Mem
import IC.Id.Fresh
import IC.Utils
import IC.Wasm.Wasmtime

{-
Interface Spec (Overview of imports):

ic0.msg_arg_data_size : () -> i32;                                          // I U Q Ry F
ic0.msg_arg_data_copy : (dst : i32, offset : i32, size : i32) -> ();        // I U Q Ry F
ic0.msg_caller_size : () -> i32;                                            // I G U Q F
ic0.msg_caller_copy : (dst : i32, offset: i32, size : i32) -> ();           // I G U Q F
ic0.msg_reject_code : () -> i32;                                            // Ry Rt
ic0.msg_reject_msg_size : () -> i32;                                        // Rt
ic0.msg_reject_msg_copy : (dst : i32, offset : i32, size : i32) -> ();      // Rt

ic0.msg_reply_data_append : (src : i32, size : i32) -> ();                  // U Q Ry Rt
ic0.msg_reply : () -> ();                                                   // U Q Ry Rt
ic0.msg_reject : (src : i32, size : i32) -> ();                             // U Q Ry Rt

ic0.msg_cycles_available : () -> i64;                                       // U Rt Ry
ic0.msg_cycles_available128 : (dst : i32) -> ();                            // U Rt Ry
ic0.msg_cycles_refunded : () -> i64;                                        // Rt Ry
ic0.msg_cycles_refunded128 : (dst : i32) -> ();                             // Rt Ry
ic0.msg_cycles_accept : (max_amount : i64) -> (amount : i64);               // U Rt Ry
ic0.msg_cycles_accept128 : (max_amount_high : i64, max_amount_low: i64, dst : i32)
                       -> ();                                               // U Rt Ry

ic0.canister_self_size : () -> i32;                                         // *
ic0.canister_self_copy : (dst : i32, offset : i32, size : i32) -> ();       // *
ic0.canister_cycle_balance : () -> i64;                                     // *
ic0.canister_cycle_balance128 : (dst : i32) -> ();                          // *
ic0.canister_status : () -> i32;                                            // *
ic0.canister_version : () -> i64;                                           // *

ic0.msg_method_name_size : () -> i32;                                       // F
ic0.msg_method_name_copy : (dst : i32, offset : i32, size : i32) -> ();     // F
ic0.accept_message : () -> ();                                              // F

ic0.call_new :                                                              // U Ry Rt T
  ( callee_src  : i32,
    callee_size : i32,
    name_src : i32,
    name_size : i32,
    reply_fun : i32,
    reply_env : i32,
    reject_fun : i32,
    reject_env : i32
  ) -> ();
ic0.call_on_cleanup : (fun : i32, env : i32) -> ();                         // U Ry Rt T
ic0.call_data_append : (src : i32, size : i32) -> ();                       // U Ry Rt T
ic0.call_cycles_add : (amount : i64) -> ();                                 // U Ry Rt T
ic0.call_cycles_add128 : (amount_high : i64, amount_low: i64) -> ();        // U Ry Rt T
ic0.call_perform : () -> ( err_code : i32 );                                // U Ry Rt T

ic0.stable_size : () -> (page_count : i32);                                 // * s
ic0.stable_grow : (new_pages : i32) -> (old_page_count : i32);              // * s
ic0.stable_write : (offset : i32, src : i32, size : i32) -> ();             // * s
ic0.stable_read : (dst : i32, offset : i32, size : i32) -> ();              // * s
ic0.stable64_size : () -> (page_count : i64);                               // * s
ic0.stable64_grow : (new_pages : i64) -> (old_page_count : i64);            // * s
ic0.stable64_write : (offset : i64, src : i64, size : i64) -> ();           // * s
ic0.stable64_read : (dst : i64, offset : i64, size : i64) -> ();            // * s

ic0.certified_data_set : (src: i32, size: i32) -> ();                       // I G U Ry Rt T
ic0.data_certificate_present : () -> i32;                                   // *
ic0.data_certificate_size : () -> i32;                                      // *
ic0.data_certificate_copy : (dst: i32, offset: i32, size: i32) -> ();       // *

ic0.time : () -> (timestamp : i64);                                         // *
ic0.global_timer_set : (timestamp : i64) -> i64;                            // I G U Ry Rt C T
ic0.performance_counter : (counter_type : i32) -> (counter : i64);          // * s
ic0.is_controller: (src: i32, size: i32) -> ( result: i32);                 // * s

ic0.debug_print : (src : i32, size : i32) -> ();                            // * s
ic0.trap : (src : i32, size : i32) -> ();                                   // * s

The comment after each function lists from where these functions may be invoked:

I: from canister_init or canister_post_upgrade
G: from canister_pre_upgrade
U: from canister_update …
Q: from canister_query …
Ry: from a reply callback
Rt: from a reject callback
C: from a cleanup callback
s: the (start) module initialization function
F: from canister_inspect_message
T: from system task (canister_heartbeat or canister_global_timer)

* = I G U Q Ry Rt C F T (NB: Not (start))

If the canister invokes a system call from somewhere else, it will trap.

Note. ic0.stable…, ic0.is_controller, ic0.debug_print, and ic0.trap are not (yet) available in the (start) module initialization function in ic-hs.
-}

data ExecutionContext = EXC_I | EXC_G | EXC_U | EXC_Q | EXC_Ry | EXC_Rt | EXC_C | EXC_F | EXC_T
  deriving Eq

instance Show ExecutionContext where
    show EXC_I  = "I"
    show EXC_G = "G"
    show EXC_U = "U"
    show EXC_Q = "Q"
    show EXC_Ry = "Ry"
    show EXC_Rt = "Rt"
    show EXC_C = "C"
    show EXC_F = "F"
    show EXC_T = "T"

-- Parameters are the data that come from the caller

data Params = Params
  { param_dat  :: Maybe Blob
  , param_caller :: Maybe EntityId
  , reject_code :: Maybe Natural
  , reject_message :: Maybe String
  , cycles_refunded :: Maybe Cycles
  }

-- The execution state is all information available to the
-- canister. Some of it is immutable (could be separated here)

data ExecutionState s = ExecutionState
  { inst :: Instance s
  , stableMem :: Memory s
  , params :: Params
  , method_name :: Maybe MethodName
  , env :: Env
  -- now the mutable parts
  , cycles_available :: Maybe Cycles
  , cycles_accepted :: Cycles
  , cycles_minted :: Cycles
  , balance :: Cycles
  , needs_to_respond :: NeedsToRespond
  , response :: Maybe Response
  , reply_data :: Blob
  , pending_call :: Maybe MethodCall
  , calls :: [MethodCall]
  , new_certified_data :: Maybe Blob
  , new_global_timer :: Maybe Word64
  , context :: ExecutionContext
  , accepted :: Bool -- for canister_inspect_message
  }


initialExecutionState :: Instance s -> Memory s -> Env -> NeedsToRespond -> ExecutionContext -> ExecutionState s
initialExecutionState inst stableMem env needs_to_respond ctxt = ExecutionState
  { inst
  , stableMem
  , params = Params Nothing Nothing Nothing Nothing Nothing
  , method_name = Nothing
  , env
  , cycles_available = Nothing
  , balance = env_balance env
  , cycles_accepted = 0
  , cycles_minted = 0
  , needs_to_respond
  , response = Nothing
  , reply_data = mempty
  , pending_call = Nothing
  , calls = mempty
  , new_certified_data = Nothing
  , new_global_timer = Nothing
  , context = ctxt
  , accepted = False
  }

-- Some bookkeeping to access the ExecutionState
--
-- We “always” have the 'STRef', but only within 'withES' is it actually
-- present.

type ESRef s = STRef s (Maybe (ExecutionState s))

newESRef :: ST s (ESRef s)
newESRef = newSTRef Nothing

-- | runs a computation with the given initial execution state
-- and returns the final execution state with it.
withES :: PrimMonad m =>
  ESRef (PrimState m) ->
  ExecutionState (PrimState m) ->
  m a -> m (a, ExecutionState (PrimState m))
withES esref es f = do
  before <- stToPrim $ readSTRef esref
  unless (isNothing before) $ error "withES with non-empty es"
  stToPrim $ writeSTRef esref $ Just es
  x <- f
  es' <- stToPrim $ readSTRef esref
  case es' of
    Nothing -> error "withES: ExecutionState lost"
    Just es' -> do
      stToPrim $ writeSTRef esref Nothing
      return (x, es')

getsES :: ESRef s -> (ExecutionState s -> b) -> HostM s b
getsES esref f = lift (readSTRef esref) >>= \case
  Nothing -> throwError "System API not available yet"
  Just es -> return (f es)

modES :: ESRef s -> (ExecutionState s -> ExecutionState s) -> HostM s ()
modES esref f = lift $ modifySTRef esref (fmap f)

appendReplyData :: ESRef s -> Blob -> HostM s ()
appendReplyData esref dat = modES esref $ \es ->
  es { reply_data = reply_data es <> dat }

setResponse :: ESRef s -> Response -> HostM s ()
setResponse esref r = modES esref $ \es ->
  es { response = Just r }

appendCall :: ESRef s -> MethodCall -> HostM s ()
appendCall esref c = modES esref $ \es ->
  es { calls = calls es ++ [c] }

getAvailable :: ESRef s -> HostM s Cycles
getAvailable esref =
  getsES esref cycles_available >>=
    maybe (throwError "no cycles available") return

getRefunded :: ESRef s -> HostM s Cycles
getRefunded esref =
  getsES esref (cycles_refunded . params)  >>=
    maybe (throwError "no cycles refunded") return

addBalance :: ESRef s -> Cycles -> HostM s ()
addBalance esref f = modES esref $ \es ->
  es { balance = balance es + f }

addAccepted :: ESRef s -> Cycles -> HostM s ()
addAccepted esref f = modES esref $ \es ->
  es { cycles_accepted = cycles_accepted es + f }

addMinted :: ESRef s -> Cycles -> HostM s ()
addMinted esref f = modES esref $ \es ->
  es { cycles_minted = cycles_minted es + f }

subtractBalance :: ESRef s -> Cycles -> HostM s ()
subtractBalance esref f = do
  current_balance <- getsES esref balance
  if f <= current_balance
  then modES esref $ \es -> es { balance = current_balance - f }
  else throwError "insufficient cycles to put on call"

subtractAvailable :: ESRef s -> Cycles -> HostM s ()
subtractAvailable esref f = do
  current <- getAvailable esref
  when (f > current) $ error "internal error: insufficient cycles to accept"
  modES esref $ \es -> es { cycles_available = Just (current - f) }

-- The System API, with all imports

-- The code is defined in the where clause to scope over the 'ESRef'

systemAPI :: forall s. ESRef s -> Imports s
systemAPI esref =
  [ toImport' "ic0" "msg_arg_data_size" [EXC_I, EXC_U, EXC_Q, EXC_Ry, EXC_F] msg_arg_data_size
  , toImport' "ic0" "msg_arg_data_copy" [EXC_I, EXC_U, EXC_Q, EXC_Ry, EXC_F] msg_arg_data_copy
  , toImport' "ic0" "msg_caller_size" [EXC_I, EXC_G, EXC_U, EXC_Q, EXC_F] msg_caller_size
  , toImport' "ic0" "msg_caller_copy" [EXC_I, EXC_G, EXC_U, EXC_Q, EXC_F] msg_caller_copy
  , toImport' "ic0" "msg_reject_code" [EXC_Ry, EXC_Rt] msg_reject_code
  , toImport' "ic0" "msg_reject_msg_size" [EXC_Rt] msg_reject_msg_size
  , toImport' "ic0" "msg_reject_msg_copy" [EXC_Rt] msg_reject_msg_copy

  , toImport' "ic0" "msg_reply_data_append" [EXC_U, EXC_Q, EXC_Ry, EXC_Rt] msg_reply_data_append
  , toImport' "ic0" "msg_reply" [EXC_U, EXC_Q, EXC_Ry, EXC_Rt] msg_reply
  , toImport' "ic0" "msg_reject" [EXC_U, EXC_Q, EXC_Ry, EXC_Rt] msg_reject

  , toImport' "ic0" "canister_self_copy" star canister_self_copy
  , toImport' "ic0" "canister_self_size" star canister_self_size
  , toImport' "ic0" "canister_status" star canister_status

  , toImport' "ic0" "msg_cycles_available" [EXC_U, EXC_Rt, EXC_Ry] msg_cycles_available
  , toImport' "ic0" "msg_cycles_refunded" [EXC_Rt, EXC_Ry] msg_cycles_refunded
  , toImport' "ic0" "msg_cycles_accept" [EXC_U, EXC_Rt, EXC_Ry] msg_cycles_accept
  , toImport' "ic0" "canister_cycle_balance" star canister_cycle_balance

  , toImport' "ic0" "msg_cycles_available128" [EXC_U, EXC_Rt, EXC_Ry] msg_cycles_available128
  , toImport' "ic0" "msg_cycles_refunded128" [EXC_Rt, EXC_Ry] msg_cycles_refunded128
  , toImport' "ic0" "msg_cycles_accept128" [EXC_U, EXC_Rt, EXC_Ry] msg_cycles_accept128
  , toImport' "ic0" "canister_cycle_balance128" star canister_cycle_balance128

  , toImport' "ic0" "call_new" [EXC_U, EXC_Ry, EXC_Rt, EXC_T] call_new
  , toImport' "ic0" "call_on_cleanup" [EXC_U, EXC_Ry, EXC_Rt, EXC_T] call_on_cleanup
  , toImport' "ic0" "call_data_append" [EXC_U, EXC_Ry, EXC_Rt, EXC_T] call_data_append
  , toImport' "ic0" "call_cycles_add" [EXC_U, EXC_Ry, EXC_Rt, EXC_T] call_cycles_add
  , toImport' "ic0" "call_cycles_add128" [EXC_U, EXC_Ry, EXC_Rt, EXC_T] call_cycles_add128
  , toImport' "ic0" "call_perform" [EXC_U, EXC_Ry, EXC_Rt, EXC_T] call_perform

  , toImport' "ic0" "stable_size" star stable_size
  , toImport' "ic0" "stable_grow" star stable_grow
  , toImport' "ic0" "stable_write" star stable_write
  , toImport' "ic0" "stable_read" star stable_read

  , toImport' "ic0" "stable64_size" star stable64_size
  , toImport' "ic0" "stable64_grow" star stable64_grow
  , toImport' "ic0" "stable64_write" star stable64_write
  , toImport' "ic0" "stable64_read" star stable64_read

  , toImport' "ic0" "certified_data_set" [EXC_I, EXC_G, EXC_U, EXC_Ry, EXC_Rt, EXC_T] certified_data_set
  , toImport' "ic0" "data_certificate_present" star data_certificate_present
  , toImport' "ic0" "data_certificate_size" star data_certificate_size
  , toImport' "ic0" "data_certificate_copy" star data_certificate_copy

  , toImport' "ic0" "msg_method_name_size" [EXC_F] msg_method_name_size
  , toImport' "ic0" "msg_method_name_copy" [EXC_F] msg_method_name_copy

  , toImport' "ic0" "accept_message" [EXC_F] accept_message
  , toImport' "ic0" "time" star get_time
  , toImport' "ic0" "performance_counter" star performance_counter
  , toImport' "ic0" "is_controller" star is_controller
  , toImport' "ic0" "global_timer_set" [EXC_I, EXC_G, EXC_U, EXC_Ry, EXC_Rt, EXC_C, EXC_T] global_timer_set
  , toImport' "ic0" "canister_version" star get_canister_version

  , toImport' "ic0" "debug_print" star debug_print
  , toImport' "ic0" "trap" star explicit_trap

  , toImport' "ic0" "mint_cycles" [EXC_U, EXC_Ry, EXC_Rt, EXC_T] mint_cycles
  ]
  where
    -- Utilities
    gets :: (ExecutionState s -> b) -> HostM s b
    gets = getsES esref

    assert_context :: String -> [ExecutionContext] -> HostM s ()
    assert_context method ctxts = do
      ctxt <- gets context
      unless (ctxt `elem` ctxts) $
        throwError $ method ++ " cannot be called in context " ++ show ctxt

    toImport' ::
        forall a b.
        (WasmArgs a, WasmArgs b) =>
        String -> String -> [ExecutionContext] -> (a -> HostM s b) -> Import s
    toImport' mod_name fun_name ctxts f =
      toImport mod_name fun_name $ \a -> do
        assert_context fun_name ctxts
        f a

    star :: [ExecutionContext]
    star = [EXC_I, EXC_G, EXC_U, EXC_Q, EXC_Ry, EXC_Rt, EXC_C, EXC_F, EXC_T]

    puts :: (ExecutionState s -> ExecutionState s) -> HostM s ()
    puts = modES esref

    copy_to_canister :: Int32 -> Int32 -> Int32 -> Blob -> HostM s ()
    copy_to_canister dst offset size blob = do
      unless (offset == 0) $
        throwError "offset /= 0 not supported"
      unless (size == fromIntegral (BS.length blob)) $
        throwError "copying less than the full blob is not supported"
      i <- getsES esref inst
      -- TODO Bounds checking
      setBytes i (fromIntegral dst) blob

    copy_from_canister :: String -> Int32 -> Int32 -> HostM s Blob
    copy_from_canister _name src size = do
      i <- gets inst
      getBytes i (fromIntegral src) size

    size_and_copy :: HostM s Blob ->
      ( () -> HostM s Int32
      , (Int32, Int32, Int32) -> HostM s ()
      )
    size_and_copy get_blob =
      ( \() ->
        get_blob >>= \blob -> return $ fromIntegral (BS.length blob)
      , \(dst, offset, size) ->
        get_blob >>= \blob -> copy_to_canister dst offset size blob
      )

    cycles_accept :: Natural -> HostM s Natural
    cycles_accept max_amount = do
      available <- getAvailable esref
      let amount = min max_amount available
      subtractAvailable esref amount
      addBalance esref amount
      addAccepted esref amount
      return amount

    -- Unsafely print
    putBytes :: BS.ByteString -> HostM s ()
    putBytes bytes =
      unsafeIOToPrim $ BSC.putStrLn $ BSC.pack "debug.print: " <> bytes

    -- The system calls (in the order of the public spec)
    -- https://sdk.dfinity.org/docs/interface-spec/index.html#_system_imports

    msg_arg_data_size :: () -> HostM s Int32
    msg_arg_data_copy :: (Int32, Int32, Int32) -> HostM s ()
    (msg_arg_data_size, msg_arg_data_copy) = size_and_copy $
        gets (param_dat . params) >>= maybe (throwError "No argument") return

    msg_caller_size :: () -> HostM s Int32
    msg_caller_copy :: (Int32, Int32, Int32) -> HostM s ()
    (msg_caller_size, msg_caller_copy) = size_and_copy $
      gets (param_caller . params)
        >>= maybe (throwError "No argument") (return . rawEntityId)

    msg_reject_code :: () -> HostM s Int32
    msg_reject_code () =
      gets (reject_code . params)
        >>= maybe (throwError "No reject code") (return . fromIntegral)

    msg_reject_msg_size :: () -> HostM s Int32
    msg_reject_msg_copy :: (Int32, Int32, Int32) -> HostM s ()
    (msg_reject_msg_size, msg_reject_msg_copy) = size_and_copy $ do
      gets (reject_message . params)
        >>= maybe (throwError "No reject code") (return . BSU.fromString)

    assert_not_responded :: HostM s ()
    assert_not_responded = do
      gets needs_to_respond >>= \case
        NeedsToRespond True -> return ()
        NeedsToRespond False  -> throwError "This call has already been responded to earlier"
      gets response >>= \case
        Nothing -> return ()
        Just  _ -> throwError "This call has already been responded to in this function"

    msg_reply_data_append :: (Int32, Int32) -> HostM s ()
    msg_reply_data_append (src, size) = do
      assert_not_responded
      bytes <- copy_from_canister "msg_reply_data_append" src size
      appendReplyData esref bytes

    msg_reply :: () -> HostM s ()
    msg_reply () = do
      assert_not_responded
      bytes <- gets reply_data
      setResponse esref (Reply bytes)

    msg_reject :: (Int32, Int32) -> HostM s ()
    msg_reject (src, size) = do
      assert_not_responded
      bytes <- copy_from_canister "msg_reject" src size
      let msg = BSU.toString bytes
      setResponse esref $ Reject (RC_CANISTER_REJECT, msg)

    canister_self_size :: () -> HostM s Int32
    canister_self_copy :: (Int32, Int32, Int32) -> HostM s ()
    (canister_self_size, canister_self_copy) = size_and_copy $
      rawEntityId <$> gets (env_self . env)

    canister_status :: () -> HostM s Int32
    canister_status () = gets (env_status . env) <&> \case
        Running -> 1
        Stopping -> 2
        Stopped -> 3

    highBits :: Natural -> Word64
    highBits = fromIntegral . (flip shiftR 64)

    lowBits :: Natural -> Word64
    lowBits = fromIntegral . (0xFFFFFFFF_FFFFFFFF .&.)

    to128le :: Natural -> BS.ByteString
    to128le n = BS.toLazyByteString $ BS.word64LE (lowBits n) <> BS.word64LE (highBits n)

    combineBitHalves :: (Word64, Word64) -> Natural
    combineBitHalves (high, low) = fromIntegral high `shiftL` 64 .|. fromIntegral low

    low64BitsOrErr :: Natural -> HostM s Word64
    low64BitsOrErr n = do
      unless (highBits n == 0) $
        throwError $ "The number of cycles does not fit in 64 bits: " ++ show n
      return $ fromIntegral $ lowBits n

    msg_cycles_refunded :: () -> HostM s Word64
    msg_cycles_refunded () =  getRefunded esref >>= low64BitsOrErr

    msg_cycles_available :: () -> HostM s Word64
    msg_cycles_available () = getAvailable esref >>= low64BitsOrErr

    msg_cycles_accept :: Word64 -> HostM s Word64
    msg_cycles_accept max_amount = cycles_accept (fromIntegral max_amount) >>= low64BitsOrErr

    canister_cycle_balance :: () -> HostM s Word64
    canister_cycle_balance () = gets balance >>= low64BitsOrErr

    msg_cycles_refunded128 :: Int32 -> HostM s ()
    msg_cycles_refunded128 dst = do
      i <- getsES esref inst
      amount <- getRefunded esref
      setBytes i (fromIntegral dst) (to128le amount)

    msg_cycles_available128 :: Int32 -> HostM s ()
    msg_cycles_available128 dst = do
      i <- getsES esref inst
      amount <- getAvailable esref
      setBytes i (fromIntegral dst) (to128le amount)

    msg_cycles_accept128 :: (Word64, Word64, Int32) -> HostM s ()
    msg_cycles_accept128 (max_amount_high, max_amount_low, dst) = do
      let max_amount = combineBitHalves (max_amount_high, max_amount_low)
      amount <- cycles_accept max_amount
      i <- getsES esref inst
      setBytes i (fromIntegral dst) (to128le amount)

    canister_cycle_balance128 :: Int32 -> HostM s ()
    canister_cycle_balance128 dst = do
      i <- getsES esref inst
      amount <- gets balance
      setBytes i (fromIntegral dst) (to128le amount)

    call_new :: ( Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32 ) -> HostM s ()
    call_new ( callee_src, callee_size, name_src, name_size
             , reply_fun, reply_env, reject_fun, reject_env ) = do
      discard_pending_call
      callee <- copy_from_canister "call_simple" callee_src callee_size
      method_name <- copy_from_canister "call_simple" name_src name_size
      let reply_closure = WasmClosure reply_fun reply_env
      let reject_closure = WasmClosure reject_fun reject_env
      setPendingCall $ MethodCall
        { call_callee = EntityId callee
        , call_method_name = BSU.toString method_name -- TODO: check for valid UTF8
        , call_arg = mempty
        , call_callback = Callback reply_closure reject_closure Nothing
        , call_transferred_cycles = 0
        }

    call_on_cleanup :: (Int32, Int32) -> HostM s ()
    call_on_cleanup (fun, env) = do
      let cleanup_closure = WasmClosure fun env
      changePendingCall $ \pc -> do
        let callback = call_callback pc
        when (isJust (cleanup_callback callback)) $
            throwError "call_on_cleanup invoked twice"
        return $ pc { call_callback = callback { cleanup_callback = Just cleanup_closure } }

    call_data_append :: (Int32, Int32) -> HostM s ()
    call_data_append (src, size) = do
      arg <- copy_from_canister "call_data_append" src size
      changePendingCall $ \pc -> return $ pc { call_arg = call_arg pc <> arg }

    call_cycles_add :: Word64 -> HostM s ()
    call_cycles_add amount = call_cycles_add128 (0, amount)

    call_cycles_add128 :: (Word64, Word64) -> HostM s ()
    call_cycles_add128 amount = do
      let cycles = combineBitHalves amount
      changePendingCall $ \pc -> do
        subtractBalance esref cycles
        return $ pc { call_transferred_cycles = call_transferred_cycles pc + cycles }

    call_perform :: () -> HostM s Int32
    call_perform () = do
      pc <- getPendingCall

      appendCall esref pc
      modES esref $ \es -> es { pending_call = Nothing }
      return 0

    -- utilities for the pending call

    setPendingCall :: MethodCall -> HostM s ()
    setPendingCall pc =
      modES esref $ \es -> es { pending_call = Just pc }

    getPendingCall :: HostM s MethodCall
    getPendingCall =
      gets pending_call >>= \case
        Nothing -> throwError "No call in process"
        Just pc -> return pc

    changePendingCall :: (MethodCall -> HostM s MethodCall) -> HostM s ()
    changePendingCall act =
      getPendingCall >>= act >>= setPendingCall

    discard_pending_call = do
      mpc <- gets pending_call
      forM_ mpc $ \pc -> addBalance esref (call_transferred_cycles pc)
      modES esref $ \es -> es { pending_call = Nothing }

    checkStableMemorySize :: HostM s ()
    checkStableMemorySize = do
      m <- gets stableMem
      n <- Mem.size m
      when (n > 65536) $
        throwError "stable memory error: cannot use 32 bit API once stable memory is above 4GiB"

    stable_size :: () -> HostM s Int32
    stable_size () = do
      checkStableMemorySize
      m <- gets stableMem
      fromIntegral <$> Mem.size m

    stable_grow :: Int32 -> HostM s Int32
    stable_grow delta = do
      checkStableMemorySize
      m <- gets stableMem
      n <- Mem.size m
      if (fromIntegral delta + n) > 65536
      then return (-1)
      else fromIntegral <$> Mem.grow m (fromIntegral delta)

    stable_write :: (Int32, Int32, Int32) -> HostM s ()
    stable_write (dst, src, size) = do
      checkStableMemorySize
      m <- gets stableMem
      i <- getsES esref inst
      blob <- getBytes i (fromIntegral src) size
      Mem.write m (fromIntegral dst) blob

    stable_read :: (Int32, Int32, Int32) -> HostM s ()
    stable_read (dst, src, size) = do
      checkStableMemorySize
      m <- gets stableMem
      i <- getsES esref inst
      blob <- Mem.read m (fromIntegral src) (fromIntegral size)
      setBytes i (fromIntegral dst) blob

    stable64_size :: () -> HostM s Word64
    stable64_size () = do
      m <- gets stableMem
      Mem.size m

    stable64_grow :: Word64 -> HostM s Word64
    stable64_grow delta = do
      m <- gets stableMem
      Mem.grow m delta

    stable64_write :: (Word64, Word64, Word64) -> HostM s ()
    stable64_write (dst, src, size) = do
      m <- gets stableMem
      i <- getsES esref inst
      blob <- getBytes i (fromIntegral src) (fromIntegral size)
      Mem.write m dst blob

    stable64_read :: (Word64, Word64, Word64) -> HostM s ()
    stable64_read (dst, src, size) = do
      m <- gets stableMem
      i <- getsES esref inst
      blob <- Mem.read m src size
      setBytes i (fromIntegral dst) blob

    certified_data_set :: (Int32, Int32) -> HostM s ()
    certified_data_set (src, size) = do
      when (size > 32) $ throwError "certified_data_set: too large"
      blob <- copy_from_canister "certified_data_set" src size
      modES esref $ \es -> es { new_certified_data = Just blob }

    data_certificate_present :: () -> HostM s Int32
    data_certificate_present () =
      gets (env_certificate . env) >>= \case
        Just _ -> return 1
        Nothing -> return 0

    data_certificate_size :: () -> HostM s Int32
    data_certificate_copy :: (Int32, Int32, Int32) -> HostM s ()
    (data_certificate_size, data_certificate_copy) = size_and_copy $
      gets (env_certificate . env) >>= \case
        Just b -> return b
        Nothing -> throwError "no certificate available"

    msg_method_name_size :: () -> HostM s Int32
    msg_method_name_copy :: (Int32, Int32, Int32) -> HostM s ()
    (msg_method_name_size, msg_method_name_copy) = size_and_copy $
      gets method_name >>=
        maybe (throwError "Cannot query method name here")
              (return . toUtf8 . T.pack)

    accept_message :: () -> HostM s ()
    accept_message () = do
      a <- gets accepted
      when a $ throwError "Message already accepted"
      modES esref $ \es -> es { accepted = True }

    get_time :: () -> HostM s Word64
    get_time () = do
        Timestamp ns <- gets (env_time . env)
        return (fromIntegral ns)

    -- TODO: implement once semantics of performance_counter is known.
    performance_counter :: Int32 -> HostM s Word64
    performance_counter _ = return 0

    is_controller :: (Int32, Int32) -> HostM s Int32
    is_controller (src, size) = do
      when (size > 29) $ throwError "ic0.is_controller: Argument does not represent a principal"
      blob <- copy_from_canister "is_controller" src size
      ctrls <- gets (env_controllers . env)
      return (if EntityId blob `elem` ctrls then 1 else 0)

    get_canister_version :: () -> HostM s Word64
    get_canister_version () = do
        ns <- gets (env_canister_version . env)
        return (fromIntegral ns)

    global_timer_set :: Word64 -> HostM s Word64
    global_timer_set ts = do
        old_timer <- gets new_global_timer
        puts $ \es -> es {new_global_timer = Just ts}
        case old_timer of
          Nothing -> gets $ fromIntegral . env_global_timer . env
          Just old_timer -> return old_timer

    debug_print :: (Int32, Int32) -> HostM s ()
    debug_print (src, size) = do
      -- TODO: This should be a non-trapping copy
      bytes <- copy_from_canister "debug_print" src size
      putBytes bytes

    explicit_trap :: (Int32, Int32) -> HostM s ()
    explicit_trap (src, size) = do
      -- TODO: This should be a non-trapping copy
      bytes <- copy_from_canister "trap" src size
      let msg = BSU.toString bytes
      throwError $ "canister trapped explicitly: " ++ msg

    mint_cycles :: Word64 -> HostM s Word64
    mint_cycles amount = do
      self <- gets (env_self . env)
      let cmc = wordToId 4
      unless (self == cmc) $ throwError $ "ic0.mint_cycles can only be executed on Cycles Minting Canister: " ++ show self ++ " != " ++ show cmc
      addBalance esref $ fromIntegral amount
      addMinted esref $ fromIntegral amount
      return amount

-- The state of an instance, consisting of
--  * the underlying Wasm state,
--  * additional remembered information like the CanisterId
--  * the 'ESRef' that the system api functions are accessing
--  * the original module (so that this ImpState can be snapshotted)

data ImpState s = ImpState
  { isESRef :: ESRef s
  , isInstance :: Instance s
  , isStableMem :: Memory s
  , isModule :: BS.ByteString
  }

rawInstantiate :: BS.ByteString -> IO (TrapOr (ImpState RealWorld))
rawInstantiate wasm_mod = do
  esref <- newESRef
  result <- initialize wasm_mod (systemAPI esref) <*> Mem.new
  case result of
    Left  err -> return $ Trap err
    Right (inst, sm) -> return $ Return $ ImpState esref inst sm wasm_mod

cantRespond :: NeedsToRespond
cantRespond = NeedsToRespond False

canRespond :: NeedsToRespond
canRespond = NeedsToRespond True

canisterActions :: ExecutionState s -> CanisterActions
canisterActions es = CanisterActions
    { set_certified_data = new_certified_data es
    , set_global_timer = fromIntegral <$> new_global_timer es
    }

type CanisterEntryPoint r = forall s. (ImpState s -> ST s r)

rawInitialize :: EntityId -> Env -> Blob -> ImpState s -> ST s (TrapOr CanisterActions)
rawInitialize caller env dat (ImpState esref inst sm wasm_mod) = do
  result <- runExceptT $ do
    let es = (initialExecutionState inst sm env cantRespond EXC_I)
              { params = Params
                  { param_dat    = Just dat
                  , param_caller = Just caller
                  , reject_code  = Nothing
                  , reject_message = Nothing
                  , cycles_refunded = Nothing
                  }
              }

    --  invoke canister_init
    withES esref es $ void $ invokeExport inst "canister_init" []

  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> return $ Return $ canisterActions es'

rawHeartbeat :: Env -> ImpState s -> ST s (TrapOr ([MethodCall], CanisterActions))
rawHeartbeat env (ImpState esref inst sm wasm_mod) = do
  result <- runExceptT $ do
    let es = (initialExecutionState inst sm env cantRespond EXC_T)

    withES esref es $ void $ invokeExport inst "canister_heartbeat" []

  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> return $ Return $
        ( calls es'
        , canisterActions es'
        )

rawGlobalTimer :: Env -> ImpState s -> ST s (TrapOr ([MethodCall], CanisterActions))
rawGlobalTimer env (ImpState esref inst sm wasm_mod) = do
  result <- runExceptT $ do
    let es = (initialExecutionState inst sm env cantRespond EXC_T)

    withES esref es $ void $ invokeExport inst "canister_global_timer" []

  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> return $ Return $
        ( calls es'
        , canisterActions es'
        )

rawPreUpgrade :: EntityId -> Env -> ImpState s -> ST s (TrapOr (CanisterActions, Blob))
rawPreUpgrade caller env (ImpState esref inst sm wasm_mod) = do
  result <- runExceptT $ do
    let es = (initialExecutionState inst sm env cantRespond EXC_G)
              { params = Params
                  { param_dat    = Nothing
                  , param_caller = Just caller
                  , reject_code  = Nothing
                  , reject_message = Nothing
                  , cycles_refunded = Nothing
                  }
              }

    withES esref es $ void $ invokeExport inst "canister_pre_upgrade" []

  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> do
        stable_mem <- Mem.serialize <$> Mem.export (stableMem es')
        return $ Return (canisterActions es', stable_mem)

rawPostUpgrade :: EntityId -> Env -> Blob -> Blob -> ImpState s -> ST s (TrapOr CanisterActions)
rawPostUpgrade caller env mem dat (ImpState esref inst sm wasm_mod) = do
  result <- runExceptT $ do
    let es = (initialExecutionState inst sm env cantRespond EXC_I)
              { params = Params
                  { param_dat    = Just dat
                  , param_caller = Just caller
                  , reject_code  = Nothing
                  , reject_message = Nothing
                  , cycles_refunded = Nothing
                  }
              }
    lift $ Mem.imp (stableMem es) (Mem.deserialize mem)

    withES esref es $ void $ invokeExport inst "canister_post_upgrade" []

  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> return $ Return (canisterActions es')

rawQuery :: MethodName -> EntityId -> Env -> Blob -> ImpState s -> ST s (TrapOr Response)
rawQuery method caller env dat (ImpState esref inst sm _) = do
  let es = (initialExecutionState inst sm env canRespond EXC_Q)
            { params = Params
                { param_dat    = Just dat
                , param_caller = Just caller
                , reject_code  = Nothing
                , reject_message = Nothing
                , cycles_refunded = Nothing
                }
            }
  result <- runExceptT $ withES esref es $
    invokeExport inst ("canister_query " ++ method) []

  case result of
    Left err -> return $ Trap err
    Right (_, es')
      | Just r <- response es' -> return $ Return r
      | otherwise -> return $ Trap "No response"

rawUpdate :: MethodName -> EntityId -> Env -> NeedsToRespond -> Cycles -> Blob -> ImpState s -> ST s (TrapOr UpdateResult)
rawUpdate method caller env needs_to_respond cycles_available dat (ImpState esref inst sm _) = do
  let es = (initialExecutionState inst sm env needs_to_respond EXC_U)
            { params = Params
                { param_dat    = Just dat
                , param_caller = Just caller
                , reject_code  = Nothing
                , reject_message = Nothing
                , cycles_refunded = Nothing
                }
            , cycles_available = Just cycles_available
            }

  result <- runExceptT $ withES esref es $
    invokeExport inst ("canister_update " ++ method) []
  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> return $ Return
        ( CallActions (calls es') (cycles_accepted es') (cycles_minted es') (response es')
        , canisterActions es'
        )

rawCallback :: Callback -> Env -> NeedsToRespond -> Cycles -> Response -> Cycles -> ImpState s -> ST s (TrapOr UpdateResult)
rawCallback callback env needs_to_respond cycles_available res refund (ImpState esref inst sm _) = do
  let params = case res of
        Reply dat ->
          Params { param_dat = Just dat, param_caller = Nothing, reject_code = Just 0, reject_message = Nothing, cycles_refunded = Just refund }
        Reject (rc, reject_message) ->
          Params { param_dat = Nothing, param_caller = Nothing, reject_code = Just (rejectCode rc), reject_message = Just reject_message, cycles_refunded = Just refund }
  let ctxt = case res of
        Reply _ -> EXC_Ry
        Reject _ -> EXC_Rt
  let es = (initialExecutionState inst sm env needs_to_respond ctxt)
            { params
            , cycles_available = Just cycles_available
            }

  let WasmClosure fun_idx env = case res of
        Reply {}  -> reply_callback callback
        Reject {} -> reject_callback callback

  result <- runExceptT $ withES esref es $
    invokeTable inst fun_idx [I32 env]
  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> return $ Return
        ( CallActions (calls es') (cycles_accepted es') (cycles_minted es') (response es')
        , canisterActions es'
        )

-- Needs to be separate from rawCallback, as it is its own transaction
rawCleanup :: WasmClosure -> Env -> ImpState s -> ST s (TrapOr ())
rawCleanup (WasmClosure fun_idx cb_env) env (ImpState esref inst sm _) = do
  let es = initialExecutionState inst sm env cantRespond EXC_C

  result <- runExceptT $ withES esref es $
    invokeTable inst fun_idx [I32 cb_env]
  case result of
    Left  err -> return $ Trap err
    Right _ -> return $ Return ()

rawInspectMessage :: MethodName -> EntityId -> Env -> Blob -> ImpState s -> ST s (TrapOr Bool)
rawInspectMessage method caller env dat (ImpState esref inst sm wasm_mod) = do
  result <- runExceptT $ do
    let es = (initialExecutionState inst sm env cantRespond EXC_F)
              { params = Params
                  { param_dat    = Just dat
                  , param_caller = Just caller
                  , reject_code  = Nothing
                  , reject_message = Nothing
                  , cycles_refunded = Nothing
                  }
              , method_name = Just method
              }

    withES esref es $ void $ invokeExport inst "canister_inspect_message" []

  case result of
    Left err -> return $ Trap err
    Right (_, es') -> return $ Return (accepted es')
