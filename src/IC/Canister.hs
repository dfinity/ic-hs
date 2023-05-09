{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IC.Canister
    ( parseCanister
    , CanisterModule(..)
    , InitFunc, UpdateFunc, QueryFunc
    , asUpdate
    )
    where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Bits
import Data.Char (chr)
import Data.Either
import Data.Either.Combinators (fromRight')
import Data.Int
import Data.List
import Control.Monad
import Data.Foldable
import Control.Monad.Except
import Codec.Compression.GZip (decompress)
import Foreign.C.String
import Numeric.Natural
import System.Environment

import IC.Types
import IC.Wasm.Winter (parseModule, exportedFunctions)
import qualified Wasm.Syntax.AST as W

import IC.Hash
import IC.Utils

import IC.CBOR.Parser
import Codec.CBOR.Term

import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Base64 as BS64
import qualified Codec.CBOR.Write as CB
import qualified IC.Runtime as R (invoke)

type InitFunc = EntityId -> Env -> Blob -> IO (TrapOr CanisterActions)
type UpdateFunc = IO (TrapOr UpdateResult)
type QueryFunc = IO (TrapOr Response)

type IsPublic = Bool

data CanisterModule = CanisterModule
  { raw_wasm :: Blob
  , raw_wasm_hash :: Blob -- just caching, it’s worth it
  , canister_id_mod :: CanisterId -- for runtime
  , init_method :: InitFunc
  , update_methods :: MethodName ↦ (EntityId -> Env -> NeedsToRespond -> Cycles -> Blob -> UpdateFunc)
  , query_methods :: MethodName ↦ (EntityId -> Env -> Blob -> QueryFunc)
  , callbacks :: Callback -> Env -> NeedsToRespond -> Cycles -> Response -> Cycles -> UpdateFunc
  , cleanup :: WasmClosure -> Env -> IO (TrapOr ())
  , pre_upgrade_method :: EntityId -> Env -> IO (TrapOr CanisterActions)
  , post_upgrade_method :: EntityId -> Env -> Blob -> IO (TrapOr CanisterActions)
  , inspect_message :: MethodName -> EntityId -> Env -> Blob -> IO (TrapOr ())
  , heartbeat :: Env -> IO (TrapOr ([MethodCall], CanisterActions))
  , canister_global_timer :: Env -> IO (TrapOr ([MethodCall], CanisterActions))
  , metadata :: T.Text ↦ (IsPublic, Blob)
  }

instance Show CanisterModule where
    show _ = "CanisterModule{...}"

decodeModule :: Blob -> Either String Blob
decodeModule bytes =
  if | asmMagic `BS.isPrefixOf` bytes -> Right bytes
     | gzipMagic `BS.isPrefixOf` bytes -> Right $ decompress bytes
     | otherwise -> throwError $ "Unsupported module encoding"
  where
    asBytes = BS.pack . map chr
    asmMagic = asBytes [0x00, 0x61, 0x73, 0x6d]
    gzipMagic = asBytes [0x1f, 0x8b, 0x08]

needsToRespond :: NeedsToRespond -> Bool
needsToRespond (NeedsToRespond b) = b

data EntryPoint = RuntimeInstantiate Blob String Env
  | RuntimeInitialize EntityId Env Blob
  | RuntimeUpdate MethodName EntityId Env NeedsToRespond Cycles Blob
  | RuntimeQuery MethodName EntityId Env Blob
  | RuntimeCallback Callback Env NeedsToRespond Cycles Response Cycles
  | RuntimeCleanup WasmClosure Env
  | RuntimePreUpgrade EntityId Env
  | RuntimePostUpgrade EntityId Env Blob
  | RuntimeInspectMessage MethodName EntityId Env Blob
  | RuntimeHeartbeat Env
  | RuntimeGlobalTimer Env

blobterm :: Blob -> Term
blobterm = TBytes . BS.toStrict

cidterm :: CanisterId -> Term
cidterm = blobterm . rawEntityId

enumterm :: T.Text -> Term -> Term
enumterm n t = TMap [(TString n, t)]

mapterm :: [(T.Text, Term)] -> Term
mapterm = TMap . map (\(n, t) -> (TString n, t))

stringterm :: String -> Term
stringterm = TString . T.pack

maybeterm :: (a -> Term) -> Maybe a -> Term
maybeterm _ Nothing = TNull
maybeterm f (Just v) = f v

timestampterm :: Timestamp -> Term
timestampterm (Timestamp t) = TInteger $ fromIntegral t

certterm :: Blob -> Term
certterm bytes = mapterm [("bytes", blobterm bytes)]

entityterm :: EntityId -> Term
entityterm = blobterm . rawEntityId

envterm :: Env -> Term
envterm (Env cid t bal status cert can_version glob_timer ctrls mem_alloc cmp_alloc freeze_thresh subnet_id subnet_type subnet_size all_subnets) = mapterm [
  ("canister_id", cidterm cid), 
  ("time", timestampterm t), 
  ("balance", cyclesterm bal), 
  ("status", stringterm $ show status), 
  ("certificate", maybeterm certterm cert), 
  ("canister_version", TInteger $ fromIntegral can_version), 
  ("global_timer", TInteger $ fromIntegral glob_timer),
  ("controllers", TList $ map entityterm $ toList ctrls),
  ("memory_allocation", TInteger $ fromIntegral mem_alloc),
  ("compute_allocation", TInteger $ fromIntegral cmp_alloc),
  ("freeze_threshold", TInteger $ fromIntegral freeze_thresh),
  ("subnet_id", entityterm subnet_id),
  ("subnet_type", stringterm $ show subnet_type),
  ("subnet_size", TInteger $ fromIntegral subnet_size),
  ("all_subnets", TList $ map entityterm all_subnets)
  ]

cbterm :: Callback -> Term
cbterm (Callback reply_closure reject_closure cleanup_closure) = mapterm [("reply_closure", closureterm reply_closure), ("reject_closure", closureterm reject_closure), ("cleanup_closure", maybeterm closureterm cleanup_closure)]

codeterm :: RejectCode -> Term
codeterm RC_SYS_FATAL           = TInteger 1
codeterm RC_SYS_TRANSIENT       = TInteger 2
codeterm RC_DESTINATION_INVALID = TInteger 3
codeterm RC_CANISTER_REJECT     = TInteger 4
codeterm RC_CANISTER_ERROR      = TInteger 5

responseterm :: Response -> Term
responseterm (Reply reply_payload) = enumterm "Reply" $ mapterm [("reply_payload", blobterm reply_payload)]
responseterm (Reject (reject_code, reject_msg)) = enumterm "Reject" $ mapterm [("reject_code", codeterm reject_code), ("reject_msg", stringterm reject_msg)]

closureterm :: WasmClosure -> Term
closureterm (WasmClosure closure_idx closure_env) = mapterm [("closure_idx", TInteger $ fromIntegral closure_idx), ("closure_env", TInteger $ fromIntegral closure_env)]

cyclesmask :: Num a => a
cyclesmask = (2^(64::Int)) - 1

cyclesterm :: Cycles -> Term
cyclesterm x = TList [TInteger $ fromIntegral $ (shiftR x 64) .&. cyclesmask, TInteger $ fromIntegral $ x .&. cyclesmask]

parseInteger :: Term -> Integer
parseInteger (TInt t) = fromIntegral t
parseInteger (TInteger t) = t
parseInteger _ = error "parseInteger: not supported"

assembleCycles :: Term -> Cycles
assembleCycles (TList [high, low]) = fromIntegral $ (shiftL (parseInteger high) 64) .|. (parseInteger low)
assembleCycles _ = error "assembleCycles: not supported"

epterm :: EntryPoint -> Term
epterm (RuntimeInstantiate mod prefix env) = enumterm "RuntimeInstantiate" $ mapterm [("module", blobterm mod), ("prefix", stringterm prefix), ("env", envterm env)]
epterm (RuntimeInitialize cid env arg) = enumterm "RuntimeInitialize" $ mapterm [("caller", cidterm cid), ("env", envterm env), ("arg", blobterm arg)]
epterm (RuntimeUpdate n cid env needs_to_respond cycles arg) = enumterm "RuntimeUpdate" $ mapterm [("method", stringterm n), ("caller", cidterm cid), ("env", envterm env), ("needs_to_respond", TBool $ needsToRespond needs_to_respond), ("cycles", cyclesterm cycles), ("arg", blobterm arg)]
epterm (RuntimeQuery n cid env arg) = enumterm "RuntimeQuery" $ mapterm [("method", stringterm n), ("caller", cidterm cid), ("env", envterm env), ("arg", blobterm arg)]
epterm (RuntimeCallback cb env needs_to_respond cycles_available response refunded_cycles) = enumterm "RuntimeCallback" $ mapterm [("callback", cbterm cb), ("env", envterm env), ("needs_to_respond", TBool $ needsToRespond needs_to_respond), ("cycles_available", cyclesterm cycles_available), ("response", responseterm response), ("refunded_cycles", cyclesterm refunded_cycles)]
epterm (RuntimeCleanup wasm_closure env) = enumterm "RuntimeCleanup" $ mapterm [("wasm_closure", closureterm wasm_closure), ("env", envterm env)]
epterm (RuntimePreUpgrade cid env) = enumterm "RuntimePreUpgrade" $ mapterm [("caller", cidterm cid), ("env", envterm env)]
epterm (RuntimePostUpgrade cid env arg) = enumterm "RuntimePostUpgrade" $ mapterm [("caller", cidterm cid), ("env", envterm env), ("arg", blobterm arg)]
epterm (RuntimeInspectMessage n cid env arg) = enumterm "RuntimeInspectMessage" $ mapterm [("method", stringterm n), ("caller", cidterm cid), ("env", envterm env), ("arg", blobterm arg)]
epterm (RuntimeHeartbeat env) = enumterm "RuntimeHeartbeat" $ mapterm [("env", envterm env)]
epterm (RuntimeGlobalTimer env) = enumterm "RuntimeGlobalTimer" $ mapterm [("env", envterm env)]

parseCanister :: CanisterId -> Blob -> Either String CanisterModule
parseCanister cid bytes = do
  decodedModule <- decodeModule bytes
  wasm_mod <- either throwError pure (parseModule decodedModule)
  let icp_sections =
        [ (icp_name, W._customPayload section)
        | section <- toList (W._moduleCustom wasm_mod)
        , Just icp_name <- pure $ T.stripPrefix "icp:" (TL.toStrict (W._customName section))
        ]
  metadata <- forM icp_sections $ \(name,content) ->
    if | Just n <- T.stripPrefix "public "  name -> return (n,(True,  content))
       | Just n <- T.stripPrefix "private " name -> return (n,(False, content))
       | otherwise -> throwError $ "Invalid custom section " <> show name

  forM_ (duplicates (map fst metadata)) $ \name ->
    throwError $ "Duplicate custom section " <> show name

  return $ CanisterModule
    { raw_wasm = bytes
    , raw_wasm_hash = sha256 bytes
    , canister_id_mod = cid
    , init_method = \caller env dat -> do 
        prefix <- getExecutablePath
        inst <- invokeToUnit cid (RuntimeInstantiate decodedModule prefix env) -- TODO: return changed env components, and pass that env below: this call may cost cycles. 
        case inst of Trap err -> return $ Trap err
                     Return () -> if "canister_init" `elem` exportedFunctions wasm_mod 
                                  then invokeToCanisterActions cid (RuntimeInitialize caller env dat)
                                  else return $ Return noCanisterActions 
    , update_methods = M.fromList
      [ (m,
        \caller env needs_to_respond cycles_available dat ->
        invoke cid (RuntimeUpdate m caller env needs_to_respond cycles_available dat))
      | n <- exportedFunctions wasm_mod
      , Just m <- pure $ stripPrefix "canister_update " n
      ]
    , query_methods = M.fromList
      [ (m, \caller env arg ->
          invokeQuery cid (RuntimeQuery m caller env arg))
      | n <- exportedFunctions wasm_mod
      , Just m <- pure $ stripPrefix "canister_query " n
      ]
    , callbacks = \cb env needs_to_respond cycles_available res refund ->
      invoke cid (RuntimeCallback cb env needs_to_respond cycles_available res refund)
    , cleanup = \cb env ->
      invokeToUnit cid (RuntimeCleanup cb env)
    , pre_upgrade_method = \caller env ->
          if "canister_pre_upgrade" `elem` exportedFunctions wasm_mod 
          then invokeToCanisterActions cid (RuntimePreUpgrade caller env)
          else return $ Return noCanisterActions
    , post_upgrade_method = \caller env dat -> do
          prefix <- getExecutablePath
          inst <- invokeToUnit cid (RuntimeInstantiate decodedModule prefix env)
          case inst of Trap err -> return $ Trap err
                       Return () -> if "canister_post_upgrade" `elem` exportedFunctions wasm_mod
                                    then invokeToCanisterActions cid (RuntimePostUpgrade caller env dat)
                                    else return $ Return noCanisterActions
    , inspect_message = \method_name caller env arg ->
          if "canister_inspect_message" `elem` exportedFunctions wasm_mod 
          then invokeToUnit cid (RuntimeInspectMessage method_name caller env arg)
          else return $ Return ()
    , heartbeat = \env -> if "canister_heartbeat" `elem` exportedFunctions wasm_mod
                          then invokeToNoCyclesResponse cid (RuntimeHeartbeat env)
                          else return $ Return ([], noCanisterActions)
    , canister_global_timer = \env-> if "canister_global_timer" `elem` exportedFunctions wasm_mod 
                                     then invokeToNoCyclesResponse cid (RuntimeGlobalTimer env)
                                     else return $ Return ([], noCanisterActions)
    , metadata = M.fromList metadata
    }

unpackBlob :: Term -> Blob
unpackBlob (TBytes b) = BS.fromStrict b
unpackBlob _ = error "unpackBlob: not supported"

unpackString :: Term -> String
unpackString (TString m) = T.unpack m
unpackString _ = error "unpackString: not supported"

parseOption :: (Term -> a) -> Term -> Maybe a
parseOption _ TNull = Nothing
parseOption f t = Just $ f t

parseInt32 :: Term -> Int32
parseInt32 (TInt x) = fromIntegral x
parseInt32 (TInteger x) = fromIntegral x
parseInt32 _ = error "parseInt32: not supported"

parseNatural :: Term -> Natural
parseNatural (TInt x) = fromIntegral x
parseNatural (TInteger x) = fromIntegral x
parseNatural _ = error "parseNatural: not supported"

parseList :: Term -> [Term]
parseList (TList ts) = ts
parseList _ = error "parseList: not supported"

parseCanisterId :: Term -> CanisterId
parseCanisterId = EntityId . unpackBlob

parseWasmClosure :: Term -> WasmClosure
parseWasmClosure t = aux $ fromRight' $ parseMap "WasmClosure" t
  where
    aux m = WasmClosure (parseInt32 $ fromRight' $ parseField "closure_idx" m) (parseInt32 $ fromRight' $ parseField "closure_env" m)

parseCallback :: Term -> Callback
parseCallback t = aux $ fromRight' $ parseMap "Callback" t
  where
    aux m = Callback (parseWasmClosure $ fromRight' $ parseField "reply_closure" m) (parseWasmClosure $ fromRight' $ parseField "reject_closure" m) (parseOption parseWasmClosure $ fromRight' $ parseField "cleanup_closure" m)

parseMethodCall :: Term -> MethodCall
parseMethodCall t = aux $ fromRight' $ parseMap "RuntimeMethodCall" t
  where
    aux m = MethodCall (parseCanisterId $ fromRight' $ parseField "call_callee" m) (unpackString $ fromRight' $ parseField "call_method_name" m) (unpackBlob $ fromRight' $ parseField "call_arg" m) (parseCallback $ fromRight' $ parseField "call_callback" m) (assembleCycles $ fromRight' $ parseField "call_transferred_cycles" m)

invoke :: CanisterId -> EntryPoint -> IO (TrapOr UpdateResult)
invoke cid ep = do
  let bytes = BSU.toString $ BS64.encode $ CB.toStrictByteString $ encodeTerm $ TMap [(TString "canister_id", TBytes $ BS.toStrict $ rawEntityId cid), (TString "entry_point", epterm ep)]
  cres <- withCString bytes R.invoke
  res <- peekCString cres
  let x = fromRight' $ parseMap "RuntimeResponse" $ fromRight' $ decodeWithoutTag $ BS.fromStrict $ fromRight "" $ BS64.decode $ BSU.fromString res
  let (TString resp, payload) = head $ fromRight' $ parseMap "CanisterResponse" $ fromRight' $ parseField "response" x
  let cycles_accept = assembleCycles $ fromRight' $ parseField "cycles_accept" x
  let cycles_mint = assembleCycles $ fromRight' $ parseField "cycles_mint" x
  let new_certified_data = parseOption unpackBlob $ fromRight' $ parseField "new_certified_data" x
  let new_global_timer = parseOption parseNatural $ fromRight' $ parseField "new_global_timer" x
  let new_calls = map parseMethodCall $ parseList $ fromRight' $ parseField "new_calls" x
  if resp == "CanisterTrap" then return $ Trap $ unpackString payload
  else do
    let rep = if resp == "CanisterReply" then Just (Reply (unpackBlob payload))
              else if resp == "CanisterReject" then Just (Reject (RC_CANISTER_REJECT, unpackString payload))
              else Nothing
    return $ Return (CallActions new_calls cycles_accept cycles_mint rep, CanisterActions new_certified_data new_global_timer)

invokeToUnit :: CanisterId -> EntryPoint -> IO (TrapOr ())
invokeToUnit cid ep = ((\_ -> ()) <$>) <$> invoke cid ep

invokeToCanisterActions :: CanisterId -> EntryPoint -> IO (TrapOr CanisterActions)
invokeToCanisterActions cid ep = (snd <$>) <$> invoke cid ep

invokeToNoCyclesResponse :: CanisterId -> EntryPoint -> IO (TrapOr ([MethodCall], CanisterActions))
invokeToNoCyclesResponse cid ep = ((\(a, b) -> (ca_new_calls a, b)) <$>) <$> invoke cid ep

invokeQuery :: CanisterId -> EntryPoint -> IO (TrapOr Response)
invokeQuery cid ep = (\res -> res >>= (response . ca_response . fst)) <$> invoke cid ep
  where
    response Nothing = Trap "Canister did not respond."
    response (Just r) = Return r

-- | Turns a query function into an update function
asUpdate ::
  (EntityId -> Env -> Blob -> QueryFunc) ->
  (EntityId -> Env -> NeedsToRespond -> Cycles -> Blob -> UpdateFunc)
asUpdate f caller env (NeedsToRespond needs_to_respond) _cycles_available dat
  | not needs_to_respond = error "asUpdate: needs_to_respond == False"
  | otherwise =
    (\res -> (\res -> (noCallActions { ca_response = Just res }, noCanisterActions)) <$> res) <$>
    f caller env dat
