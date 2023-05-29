{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module IC.Canister
    ( CanStateId

    , parseCanister
    , CanisterModule(..)
    , InitFunc, UpdateFunc, QueryFunc
    , asUpdate
    )
    where

import Codec.Serialise
import GHC.Generics
import Data.Aeson

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

import IC.Purify
import IC.Canister.Snapshot
import IC.Canister.Imp
import IC.Hash
import IC.Utils

newtype CanStateId = CanStateId Int
  deriving Show

deriving instance Generic CanStateId
instance Serialise CanStateId where

customOptions :: Options
customOptions = defaultOptions
    { sumEncoding = ObjectWithSingleField
    }

instance ToJSON CanStateId where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

-- data WasmState = WinterState CanisterSnapshot
--   | RustState CanStateId
  -- deriving Show

class WasmBackend a where
  instantiateCan :: Blob -> Env -> Blob -> IO (TrapOr a)
  preUpgradeCan :: a -> EntityId -> Env -> IO (TrapOr (CanisterActions, Blob))
  -- always return an a (WasmBackend a) so the IC state can be updated with the new identifier, if necessary
  invokeCan :: a -> EntryPoint r -> IO (TrapOr (a, r))

instance WasmBackend CanStateId where
  instantiateCan _dec_mod _env _stable_mem = error "not implemented"
  preUpgradeCan _sid _caller _env = error "not implemented"
  invokeCan _sid _ep = error "not implemented"

instance WasmBackend CanisterSnapshot where
  instantiateCan dec_mod env stable_mem = error "not implemented"
  preUpgradeCan :: CanisterSnapshot -> EntityId -> Env -> IO (TrapOr (CanisterActions, Blob))
  preUpgradeCan can_snap caller env = error "not implemented"
  invokeCan can_snap (RuntimeQuery m caller env arg) = return $ invoke can_snap (rawQuery m caller env arg)
  invokeCan can_snap (RuntimeCallback cb env needs_to_respond cycles_available res refund) = return $ invoke can_snap (rawCallback cb env needs_to_respond cycles_available res refund)
  invokeCan can_snap (RuntimeUpdate m caller env needs_to_respond cycles_available dat) = return $ _ <$> invoke can_snap (rawUpdate m caller env needs_to_respond cycles_available dat)
  invokeCan _can_snap _ = undefined

type InitFunc a = EntityId -> Env -> Blob -> IO (TrapOr (a, CanisterActions))
type UpdateFunc a = a -> IO (TrapOr (a, UpdateResult))
type QueryFunc a = a -> IO (TrapOr Response)

type IsPublic = Bool

-- parametrize canistermodule over WasmBackend a, or circumvent this with `forall a. WasmBackend a =>`
data CanisterModule = CanisterModule
  { raw_wasm :: Blob
  , raw_wasm_hash :: Blob -- just caching, it’s worth it
  , init_method :: forall a. WasmBackend a => InitFunc a
  , update_methods :: forall a. WasmBackend a => MethodName ↦ (EntityId -> Env -> NeedsToRespond -> Cycles -> Blob -> UpdateFunc a)
  , query_methods :: forall a. WasmBackend a => MethodName ↦ (EntityId -> Env -> Blob -> QueryFunc a)
  , callbacks :: forall a. WasmBackend a => Callback -> Env -> NeedsToRespond -> Cycles -> Response -> Cycles -> UpdateFunc a
  , cleanup :: forall a. WasmBackend a => WasmClosure -> Env -> a -> IO (TrapOr (a, ()))
  , pre_upgrade_method :: forall a. WasmBackend a => a -> EntityId -> Env -> IO (TrapOr (CanisterActions, Blob))
  , post_upgrade_method :: forall a. WasmBackend a => EntityId -> Env -> Blob -> Blob -> IO (TrapOr (a, CanisterActions))
  , inspect_message :: forall a. WasmBackend a => MethodName -> EntityId -> Env -> Blob -> a -> IO (TrapOr ())
  , heartbeat :: forall a. WasmBackend a => Env -> a -> IO (TrapOr (a, ([MethodCall], CanisterActions)))
  , canister_global_timer :: forall a. WasmBackend a => Env -> a -> IO (TrapOr (a, ([MethodCall], CanisterActions)))
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

data EntryPoint r where
  RuntimeInitialize :: WasmBackend a => EntityId -> Env -> Blob -> EntryPoint (a, CanisterActions)
  RuntimeUpdate :: WasmBackend a => MethodName -> EntityId -> Env -> NeedsToRespond -> Cycles -> Blob -> EntryPoint (a, UpdateResult)
  RuntimeQuery :: MethodName -> EntityId -> Env -> Blob -> EntryPoint Response
  RuntimeCallback :: Callback -> Env -> NeedsToRespond -> Cycles -> Response -> Cycles -> EntryPoint UpdateResult
  RuntimeCleanup :: WasmBackend a => WasmClosure -> Env -> EntryPoint a
  RuntimePostUpgrade :: WasmBackend a => EntityId -> Env -> Blob -> EntryPoint (a, CanisterActions)
  RuntimeInspectMessage :: MethodName -> EntityId -> Env -> Blob -> EntryPoint ()
  RuntimeHeartbeat :: WasmBackend a => Env -> EntryPoint (a, ([MethodCall], CanisterActions))
  RuntimeGlobalTimer :: WasmBackend a => Env -> EntryPoint (a, ([MethodCall], CanisterActions))

-- qhelper :: MethodName -> EntityId -> Env -> Blob -> WasmState -> IO (TrapOr Response)
-- qhelper m caller env arg wasm_state = case wasm_state of
--   WinterState w -> return $ snd <$> invoke w (rawQuery m caller env arg)
--   RustState s -> invokeQuery s (RuntimeQuery m caller env arg)

-- qhelper' :: WasmBackend a => MethodName -> EntityId -> Env -> Blob -> a -> IO (TrapOr Response)
-- qhelper' m caller env arg wasm_state = fmap snd <$> invokeCan wasm_state (RuntimeQuery m caller env arg)

parseCanister :: RuntimeMode -> Blob -> Either String CanisterModule
parseCanister mode bytes = do
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
    , init_method = \caller env dat -> undefined
        -- case mode of  WinterRuntime -> return $ returnWinterState <$>
        --                 case instantiate wasm_mod of
        --                   Trap err -> Trap err
        --                   Return wasm_state0 -> invoke wasm_state0 (rawInitialize caller env dat)
        --               RustRuntime -> do
        --                 inst <- invokeInstantiate decodedModule env ""
        --                 case inst of Trap err -> return $ Trap err
        --                              Return (s, _) -> do
        --                                 let cycles_used = 0 -- TODO
        --                                 let new_env = env { env_balance = env_balance env - cycles_used }
        --                                 returnRustState <$> invokeToCanisterActions s (RuntimeInitialize caller new_env dat)
    , update_methods = M.fromList
      [ (m, \caller env needs_to_respond cycles_available dat wasm_state ->
          fmap snd <$> invokeCan wasm_state (RuntimeUpdate m caller env needs_to_respond cycles_available dat))
      | n <- exportedFunctions wasm_mod
      , Just m <- pure $ stripPrefix "canister_update " n
      ]
    , query_methods = M.fromList
      [ (m, \caller env arg wasm_state -> fmap snd <$> invokeCan wasm_state (RuntimeQuery m caller env arg))
      | n <- exportedFunctions wasm_mod
      , Just m <- pure $ stripPrefix "canister_query " n
      ]
    , callbacks = \cb env needs_to_respond cycles_available res refund wasm_state ->
        invokeCan wasm_state (RuntimeCallback cb env needs_to_respond cycles_available res refund)
    , cleanup = \cb env wasm_state -> undefined
        -- case wasm_state of  WinterState w -> return $ returnWinterState <$> invoke w (rawCleanup cb env)
        --                     RustState s -> returnRustState <$> invokeToUnit s (RuntimeCleanup cb env)
    , pre_upgrade_method = \wasm_state caller env -> undefined
        -- case wasm_state of  WinterState w -> return $ snd <$> invoke w (rawPreUpgrade caller env)
        --                     RustState s -> invokePreUpgrade s caller env
    , post_upgrade_method = \caller env mem dat -> undefined
        -- case mode of  WinterRuntime -> return $ returnWinterState <$>
        --                 case instantiate wasm_mod of
        --                   Trap err -> Trap err
        --                   Return wasm_state0 -> invoke wasm_state0 (rawPostUpgrade caller env mem dat)
        --               RustRuntime -> do
        --                 inst <- invokeInstantiate decodedModule env mem
        --                 case inst of  Trap err -> return $ Trap err
        --                               Return (s, _) -> do
        --                                   let cycles_used = 0 -- TODO
        --                                   let new_env = env { env_balance = env_balance env - cycles_used }
        --                                   returnRustState <$> invokeToCanisterActions s (RuntimePostUpgrade caller new_env dat)
    , inspect_message = \method_name caller env arg wasm_state -> undefined
        -- case wasm_state of  WinterState w -> return $ snd <$> invoke w (rawInspectMessage method_name caller env arg)
        --                     RustState s -> invokeToNoResult s (RuntimeInspectMessage method_name caller env arg)
    , heartbeat = \env wasm_state -> undefined
        -- case wasm_state of  WinterState w -> return $ returnWinterState <$> invoke w (rawHeartbeat env)
        --                     RustState s -> returnRustState <$> invokeToNoCyclesResponse s (RuntimeHeartbeat env)
    , canister_global_timer = \env wasm_state -> undefined
        -- case wasm_state of  WinterState w -> return $ returnWinterState <$> invoke w (rawGlobalTimer env)
        --                     RustState s -> returnRustState <$> invokeToNoCyclesResponse s (RuntimeGlobalTimer env)
    , metadata = M.fromList metadata
    }


instantiate :: Module -> TrapOr CanisterSnapshot
instantiate wasm_mod =
  either Trap Return $ snd $ createMaybe $ do
    rawInstantiate wasm_mod >>= \case
      Trap err -> return ((), Left err)
      Return rs -> return ((), Right rs)

invoke :: CanisterSnapshot -> CanisterEntryPoint (TrapOr r) -> TrapOr (CanisterSnapshot, r)
invoke s f =
  case perform f s of
    (_, Trap msg) -> Trap msg
    (s', Return r) -> Return (s', r)

-- invokeWasmBox :: CanStateId -> EntryPoint -> IO (TrapOr (CanStateId, UpdateResult))
invokeWasmBox :: CanStateId -> EntryPoint r -> IO (TrapOr r)
invokeWasmBox _s _ep = error "not implemented"

-- separate from invokeWasmBox because no other EntryPoint returns stable memory
-- and we'd need to enforce the property that invokeWasmBox returns stable memory
-- if and only if the EntryPoint is PreUpgrade
-- invokePreUpgrade :: CanStateId -> EntityId -> Env -> IO (TrapOr (CanisterActions, Blob))
invokePreUpgrade :: CanStateId -> EntityId -> Env -> IO (TrapOr (CanisterActions, Blob))
invokePreUpgrade _s _caller _env = error "not implemented"

-- separate from invokeWasmBox because we don't yet have a CanStateId
-- invokeInstantiate :: Blob -> Env -> Blob -> IO (TrapOr (CanStateId, ()))
invokeInstantiate :: Blob -> Env -> Blob -> IO (TrapOr (CanStateId, ()))
invokeInstantiate _decodedModule _env _stable_mem = error "not implemented"

-- invokeToNoResult :: CanStateId -> EntryPoint -> IO (TrapOr ())
-- invokeToNoResult :: CanStateId -> EntryPoint r -> IO (TrapOr r)
-- invokeToNoResult s ep = ((\_ -> ()) <$>) <$> invokeWasmBox s ep

-- invokeToUnit :: CanStateId -> EntryPoint -> IO (TrapOr (CanStateId, ()))
-- invokeToUnit :: CanStateId -> EntryPoint r -> IO (TrapOr r)
-- invokeToUnit s ep = ((\(w, _) -> (w, ())) <$>) <$> invokeWasmBox s ep

-- invokeToCanisterActions :: CanStateId -> EntryPoint -> IO (TrapOr (CanStateId, CanisterActions))
-- invokeToCanisterActions :: CanStateId -> EntryPoint r -> IO (TrapOr r)
-- invokeToCanisterActions s ep = ((\(w, r) -> (w, snd r)) <$>) <$> invokeWasmBox s ep

-- invokeToNoCyclesResponse :: CanStateId -> EntryPoint -> IO (TrapOr (CanStateId, ([MethodCall], CanisterActions)))
-- invokeToNoCyclesResponse :: CanStateId -> EntryPoint r -> IO (TrapOr r)
-- invokeToNoCyclesResponse s ep = ((\(w, (a, b)) -> (w, (ca_new_calls a, b))) <$>) <$> invokeWasmBox s ep

-- invokeQuery :: CanStateId -> EntryPoint -> IO (TrapOr Response)
-- invokeQuery :: CanStateId -> EntryPoint r -> IO (TrapOr r)
-- invokeQuery s ep = (\res -> res >>= (\(_, r) -> response $ ca_response $ fst r)) <$> invokeWasmBox s ep
--   where
--     response Nothing = Trap "Canister did not respond."
--     response (Just r) = Return r

-- | Turns a query function into an update function
asUpdate ::
  (EntityId -> Env -> Blob -> QueryFunc a) ->
  (EntityId -> Env -> NeedsToRespond -> Cycles -> Blob -> UpdateFunc a)
asUpdate f caller env (NeedsToRespond needs_to_respond) _cycles_available dat wasm_state
  | not needs_to_respond = error "asUpdate: needs_to_respond == False"
  | otherwise =
    (\res -> (\res -> (wasm_state, (noCallActions { ca_response = Just res }, noCanisterActions))) <$> res) <$>
    f caller env dat wasm_state


