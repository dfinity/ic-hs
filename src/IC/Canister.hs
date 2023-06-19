{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module IC.Canister
    ( WasmState
    , parseCanister
    , decodeModule
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
import Control.Monad.ST
import Data.Foldable
import Control.Monad.Except
import Codec.Compression.GZip (decompress)
import qualified Data.Vector as Vec

import IC.Types
import IC.Wasm.Winter (parseModule, exportedFunctions, Module)
import qualified Wasm.Syntax.AST as W

import Wasmtime hiding (Module)

import IC.Purify
import IC.Canister.Snapshot
import IC.Canister.Imp
import IC.Hash
import IC.Utils
import UnliftIO.Exception

-- Here we can swap out the purification machinery
type WasmState = CanisterSnapshot
-- type WasmState = Replay ImpState

type InitFunc = EntityId -> Env -> Blob -> IO (TrapOr (WasmState, CanisterActions))
type UpdateFunc = WasmState -> TrapOr (WasmState, UpdateResult)
type QueryFunc = WasmState -> TrapOr Response

type IsPublic = Bool

data CanisterModule = CanisterModule
  { raw_wasm :: Blob
  , raw_wasm_hash :: Blob -- just caching, it’s worth it
  , exports_heartbeat :: Bool
  , exports_global_timer :: Bool
  , init_method :: InitFunc
  , update_methods :: MethodName ↦ (EntityId -> Env -> NeedsToRespond -> Cycles -> Blob -> UpdateFunc)
  , query_methods :: MethodName ↦ (EntityId -> Env -> Blob -> QueryFunc)
  , callbacks :: Callback -> Env -> NeedsToRespond -> Cycles -> Response -> Cycles -> UpdateFunc
  , cleanup :: WasmClosure -> Env -> WasmState -> TrapOr (WasmState, ())
  , pre_upgrade_method :: WasmState -> EntityId -> Env -> TrapOr (CanisterActions, Blob)
  , post_upgrade_method :: EntityId -> Env -> Blob -> Blob -> TrapOr (WasmState, CanisterActions)
  , inspect_message :: MethodName -> EntityId -> Env -> Blob -> WasmState -> TrapOr Bool
  , heartbeat :: Env -> WasmState -> TrapOr (WasmState, ([MethodCall], CanisterActions))
  , canister_global_timer :: Env -> WasmState -> TrapOr (WasmState, ([MethodCall], CanisterActions))
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

msg_reply :: IO (Either Trap ())
msg_reply = return $ Right ()

parseCanister :: Blob -> Either String CanisterModule
parseCanister bytes = do
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
    , exports_heartbeat = "canister_heartbeat" `elem` exportedFunctions wasm_mod
    , exports_global_timer = "canister_global_timer" `elem` exportedFunctions wasm_mod
    , init_method = \caller env dat -> do
          engine <- newEngine
          store <- newStore engine
          ctx <- storeContext store
          myModule <- case newModule engine (unsafeFromByteString $ BS.toStrict decodedModule) of
                            Left err -> putStrLn (show err) >> error ""
                            Right r -> return r
          -- the following crashes
          f <- newFunc ctx msg_reply
          r <- tryAny (newInstance ctx myModule (Vec.fromList [toExtern f])) :: IO (Either SomeException (Either Trap (Instance RealWorld)))
          -- the following "works" (produces proper request reject from Winter code)
          -- r <- tryAny (newInstance ctx myModule (Vec.fromList [])) :: IO (Either SomeException (Either Trap (Instance RealWorld)))
          -- end
          void $ case r of
            Left err -> putStrLn (show err)
            Right (Left err) -> putStrLn (show err)
            Right (Right r) -> putStrLn "ok"
          return $ case instantiate wasm_mod of
            Trap err -> Trap err
            Return wasm_state0 ->
              invoke wasm_state0 (rawInitialize caller env dat)
    , update_methods = M.fromList
      [ (m,
        \caller env needs_to_respond cycles_available dat wasm_state ->
        invoke wasm_state (rawUpdate m caller env needs_to_respond cycles_available dat))
      | n <- exportedFunctions wasm_mod
      , Just m <- pure $ stripPrefix "canister_update " n
      ]
    , query_methods = M.fromList
      [ (m, \caller env arg wasm_state ->
          snd <$> invoke wasm_state (rawQuery m caller env arg))
      | n <- exportedFunctions wasm_mod
      , Just m <- pure $ stripPrefix "canister_query " n
      ]
    , callbacks = \cb env needs_to_respond cycles_available res refund wasm_state ->
      invoke wasm_state (rawCallback cb env needs_to_respond cycles_available res refund)
    , cleanup = \cb env wasm_state ->
      invoke wasm_state (rawCleanup cb env)
    , pre_upgrade_method = \wasm_state caller env ->
          snd <$> invoke wasm_state (rawPreUpgrade caller env)
    , post_upgrade_method = \caller env mem dat ->
          case instantiate wasm_mod of
            Trap err -> Trap err
            Return wasm_state0 ->
              invoke wasm_state0 (rawPostUpgrade caller env mem dat)
    , inspect_message = \method_name caller env arg wasm_state ->
          snd <$> invoke wasm_state (rawInspectMessage method_name caller env arg)
    , heartbeat = \env wasm_state -> invoke wasm_state (rawHeartbeat env)
    , canister_global_timer = \env wasm_state -> invoke wasm_state (rawGlobalTimer env)
    , metadata = M.fromList metadata
    }

instantiate :: Module -> TrapOr WasmState
instantiate wasm_mod =
  either Trap Return $ snd $ createMaybe $ do
    rawInstantiate wasm_mod >>= \case
      Trap err -> return ((), Left err)
      Return rs -> return ((), Right rs)

invoke :: WasmState -> CanisterEntryPoint (TrapOr r) -> TrapOr (WasmState, r)
invoke s f =
  case perform f s of
    (_, Trap msg) -> Trap msg
    (s', Return r) -> Return (s', r)

-- | Turns a query function into an update function
asUpdate ::
  (EntityId -> Env -> Blob -> QueryFunc) ->
  (EntityId -> Env -> NeedsToRespond -> Cycles -> Blob -> UpdateFunc)
asUpdate f caller env (NeedsToRespond needs_to_respond) _cycles_available dat wasm_state
  | not needs_to_respond = error "asUpdate: needs_to_respond == False"
  | otherwise =
    (\res -> (wasm_state, (noCallActions { ca_response = Just res }, noCanisterActions))) <$>
    f caller env dat wasm_state
