{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
The canister interface, presented imperatively (or impurely), i.e. without rollback
-}
module IC.Canister.Imp
 ( ESRef
 , ImpState(..)
 , runESST
 , rawInitialize
 , rawInvoke
 , silently
 )
where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSU
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Except
import Data.STRef
import Data.Maybe
import Data.Int -- TODO: Should be Word32 in most cases
import Data.Word
import Numeric.Natural

import IC.Types
import IC.Funds
import IC.Wasm.Winter
import IC.Wasm.WinterMemory as Mem
import IC.Wasm.Imports
import qualified IC.Canister.Interface as CI

-- Parameters are the data that come from the caller

data Params = Params
  { param_dat  :: Maybe Blob
  , param_caller :: Maybe EntityId
  , reject_code :: Maybe Natural
  , reject_message :: Maybe String
  , funds_refunded :: Maybe Funds
  }

-- The execution state is all information available to the
-- canister. Some of it is immutable (could be separated here)

data ExecutionState s = ExecutionState
  { inst :: Instance s
  , stableMem :: Memory s
  , self_id :: CanisterId
  , params :: Params
  , env :: CI.Env
  -- now the mutable parts
  , funds_available :: Maybe Funds
  , funds_accepted :: Funds
  , balance :: Funds
  , responded :: Responded
  , response :: Maybe Response
  , reply_data :: Blob
  , pending_call :: Maybe MethodCall
  , calls :: [MethodCall]
  , new_certified_data :: Maybe Blob
  }


initialExecutionState ::
    CanisterId -> Instance s -> Memory s -> CI.Env -> Responded ->
    ExecutionState s
initialExecutionState self_id inst stableMem env responded = ExecutionState
  { inst
  , stableMem
  , self_id
  , params = Params Nothing Nothing Nothing Nothing Nothing
  , env
  , funds_available = Nothing
  , balance = CI.balance env
  , funds_accepted = no_funds
  , responded
  , response = Nothing
  , reply_data = mempty
  , pending_call = Nothing
  , calls = mempty
  , new_certified_data = Nothing
  }

-- Some bookkeeping to access the ExecutionState
--
-- We “always” have the 'STRef', but only within 'withES' is it actually
-- present.
--
-- Also: A flag to check whether we are running in silent mode or not
-- (a bit of a hack)

type ESRef s = (STRef s Bool, STRef s (Maybe (ExecutionState s)))

newESRef :: ST s (ESRef s)
newESRef = (,) <$> newSTRef True <*> newSTRef Nothing

runESST :: (forall s. ESRef s -> ST s a) -> a
runESST f = runST $ newESRef >>= f

-- | runs a computation with the given initial execution state
-- and returns the final execution state with it.
withES :: PrimMonad m =>
  ESRef (PrimState m) ->
  ExecutionState (PrimState m) ->
  m a -> m (a, ExecutionState (PrimState m))
withES (_pref, esref) es f = do
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

silently :: PrimMonad m => ESRef (PrimState m) -> m x -> m x
silently (pref, _esref) f = do
  before <- stToPrim $ readSTRef pref
  stToPrim $ writeSTRef pref False
  x <- f
  stToPrim $ writeSTRef pref before
  return x

getsES :: ESRef s -> (ExecutionState s -> b) -> HostM s b
getsES (_, esref) f = lift (readSTRef esref) >>= \case
  Nothing -> throwError "System API not available yet"
  Just es -> return (f es)

modES :: ESRef s -> (ExecutionState s -> ExecutionState s) -> HostM s ()
modES (_, esref) f = lift $ modifySTRef esref (fmap f)

appendReplyData :: ESRef s -> Blob -> HostM s ()
appendReplyData esref dat = modES esref $ \es ->
  es { reply_data = reply_data es <> dat }

setResponse :: ESRef s -> Response -> HostM s ()
setResponse esref r = modES esref $ \es ->
  es { response = Just r }

appendCall :: ESRef s -> MethodCall -> HostM s ()
appendCall esref c = modES esref $ \es ->
  es { calls = calls es ++ [c] }

getAvailable :: ESRef s -> HostM s Funds
getAvailable esref =
  getsES esref funds_available >>=
    maybe (throwError "no funds available") return

getRefunded :: ESRef s -> HostM s Funds
getRefunded esref =
  getsES esref (funds_refunded . params)  >>=
    maybe (throwError "no funds refunded") return

addBalance :: ESRef s -> Funds -> HostM s ()
addBalance esref f = modES esref $ \es ->
  es { balance = balance es `add_funds` f }

addAccepted :: ESRef s -> Funds -> HostM s ()
addAccepted esref f = modES esref $ \es ->
  es { funds_accepted = funds_accepted es `add_funds` f }

subtractBalance :: ESRef s -> Funds -> HostM s ()
subtractBalance esref f = do
  current_balance <- getsES esref balance
  if f `le_funds` current_balance
  then modES esref $ \es -> es { balance = current_balance `sub_funds` f }
  else throwError "insufficient funds to put on call"

subtractAvailable :: ESRef s -> Funds -> HostM s ()
subtractAvailable esref f = do
  current <- getAvailable esref
  if f `le_funds` current
  then modES esref $ \es -> es { funds_available = Just (current `sub_funds` f) }
  else throwError "insufficient funds to accept"

-- The System API, with all imports

-- The code is defined in the where clause to scope over the 'ESRef'

systemAPI :: forall s. ESRef s -> Imports s
systemAPI esref =
  [ toImport "ic0" "msg_arg_data_size" msg_arg_data_size
  , toImport "ic0" "msg_arg_data_copy" msg_arg_data_copy
  , toImport "ic0" "msg_caller_size" msg_caller_size
  , toImport "ic0" "msg_caller_copy" msg_caller_copy
  , toImport "ic0" "msg_reject_code" msg_reject_code
  , toImport "ic0" "msg_reject_msg_size" msg_reject_msg_size
  , toImport "ic0" "msg_reject_msg_copy" msg_reject_msg_copy

  , toImport "ic0" "msg_reply_data_append" msg_reply_data_append
  , toImport "ic0" "msg_reply" msg_reply
  , toImport "ic0" "msg_reject" msg_reject

  , toImport "ic0" "canister_self_copy" canister_self_copy
  , toImport "ic0" "canister_self_size" canister_self_size

  , toImport "ic0" "msg_funds_available" msg_funds_available
  , toImport "ic0" "msg_funds_refunded" msg_funds_refunded
  , toImport "ic0" "msg_funds_accept" msg_funds_accept
  , toImport "ic0" "canister_balance" canister_balance

  , toImport "ic0" "call_new" call_new
  , toImport "ic0" "call_data_append" call_data_append
  , toImport "ic0" "call_funds_add" call_funds_add
  , toImport "ic0" "call_perform" call_perform

  , toImport "ic0" "stable_size" stable_size
  , toImport "ic0" "stable_grow" stable_grow
  , toImport "ic0" "stable_write" stable_write
  , toImport "ic0" "stable_read" stable_read

  , toImport "ic0" "certified_data_set" certified_data_set
  , toImport "ic0" "data_certificate_present" data_certificate_present
  , toImport "ic0" "data_certificate_size" data_certificate_size
  , toImport "ic0" "data_certificate_copy" data_certificate_copy

  , toImport "ic0" "time" get_time

  , toImport "ic0" "debug_print" debug_print
  , toImport "ic0" "trap" explicit_trap
  ]
  where
    -- Utilities
    gets :: (ExecutionState s -> b) -> HostM s b
    gets = getsES esref

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

    -- Unsafely print (if not in silent mode)
    putBytes :: BS.ByteString -> HostM s ()
    putBytes bytes =
      stToPrim (readSTRef (fst esref)) >>= \case
        True -> unsafeIOToPrim $ BSC.putStrLn $ BSC.pack "debug.print: " <> bytes
        False -> return ()

    -- The system calls (in the order of the public spec)
    -- https://docs.dfinity.systems/spec/public/#_system_imports

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
      gets responded >>= \case
        Responded False -> return ()
        Responded True  -> throwError "This call has already been responded to earlier"
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
      rawEntityId <$> gets self_id

    msg_funds_refunded :: (Int32, Int32) -> HostM s Word64
    msg_funds_refunded (unit_src, unit_size) = do
      unit <- copy_from_canister "msg_funds_refunded" unit_src unit_size
      fromIntegral . (!$ unit) <$> getRefunded esref

    msg_funds_available :: (Int32, Int32) -> HostM s Word64
    msg_funds_available (unit_src, unit_size) = do
      unit <- copy_from_canister "msg_funds_available" unit_src unit_size
      fromIntegral . (!$ unit) <$> getAvailable esref

    msg_funds_accept :: (Int32, Int32, Word64) -> HostM s ()
    msg_funds_accept (unit_src, unit_size, amount) = do
      unit <- copy_from_canister "msg_funds_accept" unit_src unit_size
      let accepted = unit_funds unit (fromIntegral amount)
      subtractAvailable esref accepted
      addBalance esref accepted
      addAccepted esref accepted

    canister_balance :: (Int32, Int32) -> HostM s Word64
    canister_balance (unit_src, unit_size) = do
      unit <- copy_from_canister "canister_balance" unit_src unit_size
      fromIntegral . (!$ unit) <$> gets balance

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
        , call_callback = Callback reply_closure reject_closure
        , call_transferred_funds = no_funds
        }

    call_data_append :: (Int32, Int32) -> HostM s ()
    call_data_append (src, size) = do
      arg <- copy_from_canister "call_data_append" src size
      changePendingCall $ \pc -> return $ pc { call_arg = call_arg pc <> arg }

    call_funds_add :: (Int32, Int32, Word64) -> HostM s ()
    call_funds_add (unit_src, unit_size, amount) = do
      unit <- copy_from_canister "call_funds_add" unit_src unit_size
      let funds = unit_funds unit (fromIntegral amount)
      changePendingCall $ \pc -> do
        subtractBalance esref funds
        return $ pc { call_transferred_funds = call_transferred_funds pc `add_funds` funds }

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
      forM_ mpc $ \pc -> addBalance esref (call_transferred_funds pc)
      modES esref $ \es -> es { pending_call = Nothing }

    stable_size :: () -> HostM s Int32
    stable_size () = do
      m <- gets stableMem
      Mem.size m

    stable_grow :: Int32 -> HostM s Int32
    stable_grow delta = do
      m <- gets stableMem
      Mem.grow m delta

    stable_write :: (Int32, Int32, Int32) -> HostM s ()
    stable_write (dst, src, size) = do
      m <- gets stableMem
      i <- getsES esref inst
      blob <- getBytes i (fromIntegral src) size
      Mem.write m (fromIntegral dst) blob

    stable_read :: (Int32, Int32, Int32) -> HostM s ()
    stable_read (dst, src, size) = do
      m <- gets stableMem
      i <- getsES esref inst
      blob <- Mem.read m (fromIntegral src) size
      setBytes i (fromIntegral dst) blob

    certified_data_set :: (Int32, Int32) -> HostM s ()
    certified_data_set (src, size) = do
      when (size > 32) $ throwError "certified_data_set: too large"
      blob <- copy_from_canister "certified_data_set" src size
      modES esref $ \es -> es { new_certified_data = Just blob }

    data_certificate_present :: () -> HostM s Int32
    data_certificate_present () =
      gets (CI.certificate . env) >>= \case
        Just _ -> return 1
        Nothing -> return 0

    data_certificate_size :: () -> HostM s Int32
    data_certificate_copy :: (Int32, Int32, Int32) -> HostM s ()
    (data_certificate_size, data_certificate_copy) = size_and_copy $
      gets (CI.certificate . env) >>= \case
        Just b -> return b
        Nothing -> throwError "no certificate available"

    get_time :: () -> HostM s Word64
    get_time () = do
        Timestamp ns <- gets (CI.time . env)
        return (fromIntegral ns)

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

-- The state of an instance, consistig of the underlying Wasm state,
-- additional remembered information like the CanisterId
-- and the 'ESRef' that the system api functions are accessing

data ImpState s = ImpState
  { isESRef :: ESRef s
  , isCanisterId :: CanisterId
  , isInstance :: Instance s
  , isStableMem :: Memory s
  }

rawInitialize :: ESRef s -> CanisterId -> Module -> ST s (TrapOr (ImpState s))
rawInitialize esref cid wasm_mod = do
  result <- runExceptT $ (,)
    <$> initialize wasm_mod (systemAPI esref)
    <*> Mem.new
  case result of
    Left  err -> return $ Trap err
    Right (inst, sm) -> return $ Return $ ImpState esref cid inst sm

rawInvoke :: ImpState s -> CI.CanisterMethod r -> ST s (TrapOr r)
rawInvoke is (CI.Initialize wasm_mod caller env dat) =
    rawInitializeMethod is wasm_mod caller env dat
rawInvoke is (CI.Query name caller env dat) =
    rawQueryMethod is name caller env dat
rawInvoke is (CI.Update name caller env responded funds_available dat) =
    rawUpdateMethod is name caller env responded funds_available dat
rawInvoke is (CI.Callback cb env responded funds_received res refund) =
    rawCallbackMethod is cb env responded funds_received res refund
rawInvoke is (CI.PreUpgrade wasm_mod caller env) =
    rawPreUpgrade is wasm_mod caller env
rawInvoke is (CI.PostUpgrade wasm_mod caller env mem dat) =
    rawPostUpgrade is wasm_mod caller env mem dat

cantRespond :: Responded
cantRespond = Responded True

canRespond :: Responded
canRespond = Responded False

canisterActions :: ExecutionState s -> CanisterActions
canisterActions es = CanisterActions
    { set_certified_data = new_certified_data es
    }

rawInitializeMethod :: ImpState s -> Module -> EntityId -> CI.Env -> Blob -> ST s (TrapOr CanisterActions)
rawInitializeMethod (ImpState esref cid inst sm) wasm_mod caller env dat = do
  result <- runExceptT $ do
    let es = (initialExecutionState cid inst sm env cantRespond)
              { params = Params
                  { param_dat    = Just dat
                  , param_caller = Just caller
                  , reject_code  = Nothing
                  , reject_message = Nothing
                  , funds_refunded = Nothing
                  }
              }

    --  invoke canister_init
    if "canister_init" `elem` exportedFunctions wasm_mod
    then withES esref es $ void $ invokeExport inst "canister_init" []
    else return ((), es)

  case result of
    Left  err -> return $ Trap err
    Right (_, es')
        | null (calls es') -> return $ Return $ canisterActions es'
        | otherwise        -> return $ Trap "cannot call from init"

rawPreUpgrade :: ImpState s -> Module -> EntityId -> CI.Env -> ST s (TrapOr (CanisterActions, Blob))
rawPreUpgrade (ImpState esref cid inst sm) wasm_mod caller env = do
  result <- runExceptT $ do
    let es = (initialExecutionState cid inst sm env cantRespond)
              { params = Params
                  { param_dat    = Nothing
                  , param_caller = Just caller
                  , reject_code  = Nothing
                  , reject_message = Nothing
                  , funds_refunded = Nothing
                  }
              }

    if "canister_pre_upgrade" `elem` exportedFunctions wasm_mod
    then withES esref es $ void $ invokeExport inst "canister_pre_upgrade" []
    else return ((), es)

  case result of
    Left  err -> return $ Trap err
    Right (_, es')
        | null (calls es') -> do
            stable_mem <- Mem.export (stableMem es')
            return $ Return (canisterActions es', stable_mem)
        | otherwise        -> return $ Trap "cannot call from pre_upgrade"

rawPostUpgrade :: ImpState s -> Module -> EntityId -> CI.Env -> Blob -> Blob -> ST s (TrapOr CanisterActions)
rawPostUpgrade (ImpState esref cid inst sm) wasm_mod caller env mem dat = do
  result <- runExceptT $ do
    let es = (initialExecutionState cid inst sm env cantRespond)
              { params = Params
                  { param_dat    = Just dat
                  , param_caller = Just caller
                  , reject_code  = Nothing
                  , reject_message = Nothing
                  , funds_refunded = Nothing
                  }
              }
    lift $ Mem.imp (stableMem es) mem

    if "canister_post_upgrade" `elem` exportedFunctions wasm_mod
    then withES esref es $ void $ invokeExport inst "canister_post_upgrade" []
    else return ((), es)

  case result of
    Left  err -> return $ Trap err
    Right (_, es')
        | null (calls es') -> return $ Return (canisterActions es')
        | otherwise        -> return $ Trap "cannot call from post_upgrade"

rawQueryMethod :: ImpState s -> MethodName -> EntityId -> CI.Env -> Blob -> ST s (TrapOr Response)
rawQueryMethod (ImpState esref cid inst sm) method caller env dat = do
  let es = (initialExecutionState cid inst sm env canRespond)
            { params = Params
                { param_dat    = Just dat
                , param_caller = Just caller
                , reject_code  = Nothing
                , reject_message = Nothing
                , funds_refunded = Nothing
                }
            }
  result <- runExceptT $ withES esref es $
    invokeExport inst ("canister_query " ++ method) []

  case result of
    Left err -> return $ Trap err
    Right (_, es')
      | not (null (calls es')) -> return $ Trap "cannot call from query"
      | Just r <- response es' -> return $ Return r
      | otherwise -> return $ Trap "No response"

rawUpdateMethod :: ImpState s -> MethodName -> EntityId -> CI.Env -> Responded -> Funds -> Blob -> ST s (TrapOr UpdateResult)
rawUpdateMethod (ImpState esref cid inst sm) method caller env responded funds_available dat = do
  let es = (initialExecutionState cid inst sm env responded)
            { params = Params
                { param_dat    = Just dat
                , param_caller = Just caller
                , reject_code  = Nothing
                , reject_message = Nothing
                , funds_refunded = Nothing
                }
            , funds_available = Just funds_available
            }

  result <- runExceptT $ withES esref es $
    invokeExport inst ("canister_update " ++ method) []
  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> return $ Return
        ( CallActions (calls es') (funds_accepted es') (response es')
        , canisterActions es'
        )

rawCallbackMethod :: ImpState s -> Callback -> CI.Env -> Responded -> Funds -> Response -> Funds -> ST s (TrapOr UpdateResult)
rawCallbackMethod (ImpState esref cid inst sm) callback env responded funds_available res refund = do
  let params = case res of
        Reply dat ->
          Params { param_dat = Just dat, param_caller = Nothing, reject_code = Just 0, reject_message = Nothing, funds_refunded = Just refund }
        Reject (rc, reject_message) ->
          Params { param_dat = Nothing, param_caller = Nothing, reject_code = Just (rejectCode rc), reject_message = Just reject_message, funds_refunded = Just refund }
  let es = (initialExecutionState cid inst sm env responded)
            { params
            , funds_available = Just funds_available
            }

  let WasmClosure fun_idx env = case res of
        Reply {}  -> reply_callback callback
        Reject {} -> reject_callback callback

  result <- runExceptT $ withES esref es $
    invokeTable inst fun_idx [I32 env]
  case result of
    Left  err -> return $ Trap err
    Right (_, es') -> return $ Return
        ( CallActions (calls es') (funds_accepted es') (response es')
        , canisterActions es'
        )

