{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.List
import Control.Monad
import Data.Foldable
import Control.Monad.Except
import Codec.Compression.GZip (decompress)

import IC.Types
import IC.Wasm.Winter (parseModule, exportedFunctions, Module)
import qualified Wasm.Syntax.AST as W

import IC.Hash
import IC.Utils

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
        inst <- instantiate cid wasm_mod
        case inst of Trap err -> return $ Trap err
                     Return () -> invokeActions cid (RuntimeInitialize caller env dat)
    , update_methods = M.fromList
      [ (m,
        \caller env needs_to_respond cycles_available dat ->
        invokeUpdate cid (RuntimeUpdate m caller env needs_to_respond cycles_available dat))
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
      invokeCallActions cid (RuntimeCallback cb env needs_to_respond cycles_available res refund)
    , cleanup = \cb env ->
      invokeNoActions cid (RuntimeCleanup cb env)
    , pre_upgrade_method = \caller env ->
          invokeActions cid (RuntimePreUpgrade caller env)
    , post_upgrade_method = \caller env dat -> do
          inst <- instantiate cid wasm_mod
          case inst of Trap err -> return $ Trap err
                       Return () -> invokeActions cid (RuntimePostUpgrade caller env dat)
    , inspect_message = \method_name caller env arg ->
          invokeNoActions cid (RuntimeInspectMessage method_name caller env arg)
    , heartbeat = \env -> invokeCalls cid (RuntimeHeartbeat env)
    , canister_global_timer = \env-> invokeCalls cid (RuntimeGlobalTimer env)
    , metadata = M.fromList metadata
    }

instantiate :: CanisterId -> Module -> IO (TrapOr ())
instantiate _ _wasm_mod = error "not implemented"

invokeNoActions :: CanisterId -> EntryPoint -> IO (TrapOr ())
invokeNoActions _ _ = error "not implemented"

invokeActions :: CanisterId -> EntryPoint -> IO (TrapOr CanisterActions)
invokeActions _ _ = error "not implemented"

invokeCalls :: CanisterId -> EntryPoint -> IO (TrapOr ([MethodCall], CanisterActions))
invokeCalls _ _ = error "not implemented"

invokeCallActions :: CanisterId -> EntryPoint -> IO (TrapOr (CallActions, CanisterActions))
invokeCallActions _ _ = error "not implemented"

invokeUpdate :: CanisterId -> EntryPoint -> IO (TrapOr UpdateResult)
invokeUpdate _ _ = error "not implemented"

invokeQuery :: CanisterId -> EntryPoint -> IO (TrapOr Response)
invokeQuery _ _ = error "not implemented"

-- | Turns a query function into an update function
asUpdate ::
  (EntityId -> Env -> Blob -> QueryFunc) ->
  (EntityId -> Env -> NeedsToRespond -> Cycles -> Blob -> UpdateFunc)
asUpdate f caller env (NeedsToRespond needs_to_respond) _cycles_available dat
  | not needs_to_respond = error "asUpdate: needs_to_respond == False"
  | otherwise =
    (\res -> (\res -> (noCallActions { ca_response = Just res }, noCanisterActions)) <$> res) <$>
    f caller env dat
