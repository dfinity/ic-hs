{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (chr)
import Data.List
import Control.Monad
import Data.Foldable
import Control.Monad.Except
import Codec.Compression.GZip (decompress)
import qualified Data.Vector as Vec

import IC.Types
import IC.Wasm.Winter (parseModule, exportedFunctions, Module)
import qualified Wasm.Syntax.AST as W

-- import Wasmtime hiding (Module)

import IC.Purify
import IC.Canister.Snapshot
import IC.Canister.Imp
import IC.Hash
import IC.Utils

import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)


import Foreign.Marshal.Alloc (alloca, free, malloc, mallocBytes)
import Foreign.Ptr (Ptr, castPtr, nullFunPtr, nullPtr, FunPtr)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Foreign.Storable
import Data.Word
import Foreign.C.Types

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

data C'wasm_engine_t = C'wasm_engine_t
data C'wasmtime_store = C'wasmtime_store
type C'wasmtime_store_t = C'wasmtime_store
data C'wasmtime_context = C'wasmtime_context
type C'wasmtime_context_t = C'wasmtime_context
data C'wasmtime_module = C'wasmtime_module
type C'wasmtime_module_t = C'wasmtime_module
data C'wasmtime_error = C'wasmtime_error
type C'wasmtime_error_t = C'wasmtime_error
data C'wasm_trap_t = C'wasm_trap_t
data C'wasmtime_extern_t = C'wasmtime_extern_t

data C'wasmtime_instance = C'wasmtime_instance{
  c'wasmtime_instance'store_id :: Word64,
  c'wasmtime_instance'index :: CSize
} deriving (Eq,Show)
instance Storable C'wasmtime_instance where
  sizeOf _ = 16
  alignment _ = 8
  peek _p = do
    v0 <- peekByteOff _p 0
    v1 <- peekByteOff _p 8
    return $ C'wasmtime_instance v0 v1
  poke _p (C'wasmtime_instance v0 v1) = do
    pokeByteOff _p 0 v0
    pokeByteOff _p 8 v1
    return ()

type C'wasmtime_instance_t = C'wasmtime_instance

foreign import ccall unsafe "wasm_engine_new" c'wasm_engine_new
  :: IO (Ptr C'wasm_engine_t)

foreign import ccall unsafe "wasmtime_store_new" c'wasmtime_store_new
  :: Ptr C'wasm_engine_t -> Ptr a -> FunPtr (Ptr a -> IO ()) -> IO (Ptr C'wasmtime_store_t)

foreign import ccall unsafe "wasmtime_store_context" c'wasmtime_store_context
  :: Ptr C'wasmtime_store_t -> IO (Ptr C'wasmtime_context_t)

foreign import ccall unsafe "wasmtime_module_new" c'wasmtime_module_new
  :: Ptr C'wasm_engine_t -> Ptr Word8 -> CSize -> Ptr (Ptr C'wasmtime_module_t) -> IO (Ptr C'wasmtime_error_t)

foreign import ccall unsafe "wasmtime_instance_new" c'wasmtime_instance_new
  :: Ptr C'wasmtime_context_t -> Ptr C'wasmtime_module_t -> Ptr C'wasmtime_extern_t -> CSize -> Ptr C'wasmtime_instance_t -> Ptr (Ptr C'wasm_trap_t) -> IO (Ptr C'wasmtime_error_t)

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
          hSetBuffering stdout NoBuffering

          engine_ptr <- c'wasm_engine_new
          wasmtime_store_ptr <- c'wasmtime_store_new engine_ptr nullPtr nullFunPtr
          ctx_ptr <- c'wasmtime_store_context wasmtime_store_ptr
          let wasm = [0,97,115,109,1,0,0,0] :: [Word8]
              n = length wasm
          p <- mallocBytes n
          pokeArray p wasm

          module_ptr_ptr <- malloc
          error_ptr <-
            c'wasmtime_module_new
              engine_ptr
              p
              (fromIntegral n)
              module_ptr_ptr

          print ("c'wasmtime_module_new: error_ptr", error_ptr)

          mod_ptr <- peek module_ptr_ptr

          putStrLn "Creating Instance ..."
          (inst_ptr :: Ptr C'wasmtime_instance_t) <- malloc
          (trap_ptr_ptr :: Ptr (Ptr C'wasm_trap_t)) <- malloc

          error_ptr <-
            c'wasmtime_instance_new
              ctx_ptr
              mod_ptr
              nullPtr
              0
              inst_ptr
              trap_ptr_ptr

          print ("c'wasmtime_instance_new: error_ptr", error_ptr)

          putStrLn "done"

          trap_ptr <- peek trap_ptr_ptr

          putStrLn "after peek"

          if trap_ptr == nullPtr
            then error "TODO"
            else do
              putStrLn "Received trap:"
              print trap_ptr
              error "TODO"

          putStrLn "The impossible happened!"

          error "another error!"

          -- void $ case r of
          --   Left trap -> do
          --     putStrLn "Received trap:"
          --     print trap
          --   Right _ -> putStrLn "ok"
          -- return $ case instantiate wasm_mod of
          --   Trap err -> Trap err
          --   Return wasm_state0 ->
          --     invoke wasm_state0 (rawInitialize caller env dat)
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
