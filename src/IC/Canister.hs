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
import Data.Char (chr)
import Data.Either
import Data.List
import Control.Monad
import Data.Foldable
import Control.Monad.Except
import Codec.Compression.GZip (decompress)
import Foreign.C.String

import IC.Types
import IC.Wasm.Winter (parseModule, exportedFunctions)
import qualified Wasm.Syntax.AST as W

import IC.Hash
import IC.Utils

import Codec.Serialise
import GHC.Generics

import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy.UTF8 as BSLU
import qualified Data.ByteString.Base64 as BS64
import qualified Codec.CBOR.Write as CB
import qualified IC.Runtime as R (instantiate, invoke)

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

data EntryPoint = RuntimeInitialize EntityId Env Blob
  | RuntimeUpdate MethodName EntityId Env NeedsToRespond Cycles Blob
  | RuntimeQuery MethodName EntityId Env Blob
  | RuntimeCallback Callback Env NeedsToRespond Cycles Response Cycles
  | RuntimeCleanup WasmClosure Env
  | RuntimePreUpgrade EntityId Env
  | RuntimePostUpgrade EntityId Env Blob
  | RuntimeInspectMessage MethodName EntityId Env Blob
  | RuntimeHeartbeat Env
  | RuntimeGlobalTimer Env

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
        inst <- instantiate cid decodedModule
        case inst of Trap err -> return $ Trap err
                     Return () -> invokeToCanisterActions cid (RuntimeInitialize caller env dat)
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
          invokeToCanisterActions cid (RuntimePreUpgrade caller env)
    , post_upgrade_method = \caller env dat -> do
          inst <- instantiate cid decodedModule
          case inst of Trap err -> return $ Trap err
                       Return () -> invokeToCanisterActions cid (RuntimePostUpgrade caller env dat)
    , inspect_message = \method_name caller env arg ->
          invokeToUnit cid (RuntimeInspectMessage method_name caller env arg)
    , heartbeat = \env -> invokeToNoCyclesResponse cid (RuntimeHeartbeat env)
    , canister_global_timer = \env-> invokeToNoCyclesResponse cid (RuntimeGlobalTimer env)
    , metadata = M.fromList metadata
    }

data RuntimeInstantiate = RuntimeInstantiate (CanisterId, Blob) deriving Show

deriving instance Serialise EntityId

deriving instance Generic RuntimeInstantiate
instance Serialise RuntimeInstantiate where

deriving instance Generic Timestamp
instance Serialise Timestamp where

deriving instance Generic Status
instance Serialise Status where

deriving instance Generic NeedsToRespond
instance Serialise NeedsToRespond where

deriving instance Generic Callback
instance Serialise Callback where

deriving instance Generic WasmClosure
instance Serialise WasmClosure where

deriving instance Generic Response
instance Serialise Response where

deriving instance Generic RejectCode
instance Serialise RejectCode where

deriving instance Generic Env
instance Serialise Env where

deriving instance Generic EntryPoint
instance Serialise EntryPoint where

instantiate :: CanisterId -> Blob -> IO (TrapOr ())
instantiate cid wasm_mod = do
  let bytes = BSU.toString $ BS64.encode $ CB.toStrictByteString $ encode $ (cid, wasm_mod)
  cres <- withCString bytes R.instantiate
  res <- peekCString cres
  putStrLn $ res
  return $ Return ()

invoke :: CanisterId -> EntryPoint -> IO (TrapOr UpdateResult)
invoke cid ep = do
  let bytes = BSU.toString $ BS64.encode $ CB.toStrictByteString $ encode (cid, ep)
  cres <- withCString bytes R.invoke
  res <- peekCString cres
  putStrLn $ res
  let prefix_reply = "ok: reply: "
  let prefix_reject = "ok: reject: "
  let resp = if isPrefixOf prefix_reply res then Just (Reply $ BS.fromStrict $ fromRight "" (BS64.decode (BS.toStrict $ BS.drop (fromIntegral $ length prefix_reply) (BSLU.fromString res))))
             else if isPrefixOf prefix_reject res then Just (Reject (RC_CANISTER_REJECT, drop (length prefix_reject) res))
             else Nothing
  return $ Return (CallActions [] 0 0 resp, noCanisterActions)

invokeToUnit :: CanisterId -> EntryPoint -> IO (TrapOr ())
invokeToUnit cid ep = ((\_ -> ()) <$>) <$> invoke cid ep

invokeToCanisterActions :: CanisterId -> EntryPoint -> IO (TrapOr CanisterActions)
invokeToCanisterActions cid ep = (snd <$>) <$> invoke cid ep

invokeToNoCyclesResponse :: CanisterId -> EntryPoint -> IO (TrapOr ([MethodCall], CanisterActions))
invokeToNoCyclesResponse cid ep = ((\(a, b) -> (ca_new_calls a, b)) <$>) <$> invoke cid ep

invokeQuery :: CanisterId -> EntryPoint -> IO (TrapOr Response)
invokeQuery cid ep = (\res -> res >>= (response . ca_response . fst)) <$> invoke cid ep
  where
    response Nothing = Trap ""
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
