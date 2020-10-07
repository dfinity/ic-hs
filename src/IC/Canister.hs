{-# LANGUAGE TypeOperators #-}

module IC.Canister
    ( WasmState
    , parseCanister
    , CanisterModule(..)
    , InitFunc, UpdateFunc, QueryFunc
    , asUpdate
    )
    where

import qualified Data.Map as M
import Data.List

import IC.Types
import IC.Funds
import IC.Wasm.Winter (parseModule, exportedFunctions, Module)

import qualified IC.Canister.Interface as CI
-- Here we can swap out the persistence implementation
import IC.Canister.Persisted

type InitFunc = CanisterId -> EntityId -> CI.Env -> Blob -> TrapOr WasmState
type UpdateFunc = WasmState -> TrapOr (WasmState, UpdateResult)
type QueryFunc = WasmState -> TrapOr Response

data CanisterModule = CanisterModule
  { init_method :: InitFunc
  , update_methods :: MethodName ↦ (EntityId -> CI.Env -> Responded -> Funds -> Blob -> UpdateFunc)
  , query_methods :: MethodName ↦ (EntityId -> CI.Env -> Blob -> QueryFunc)
  , callbacks :: Callback -> CI.Env -> Responded -> Funds -> Response -> Funds -> UpdateFunc
  , pre_upgrade_method :: WasmState -> EntityId -> CI.Env -> TrapOr Blob
  , post_upgrade_method :: CanisterId -> EntityId -> CI.Env -> Blob -> Blob -> TrapOr WasmState
  }

instance Show CanisterModule where
    show _ = "CanisterModule{...}"

parseCanister :: Blob -> Either String CanisterModule
parseCanister bytes =
  case parseModule bytes of
    Left  err -> Left err
    Right wasm_mod -> Right $ concreteToAbstractModule wasm_mod

concreteToAbstractModule :: Module -> CanisterModule
concreteToAbstractModule wasm_mod = CanisterModule
  { init_method = \cid caller env dat -> initialize wasm_mod cid caller env dat
  , update_methods = M.fromList
    [ (m,
      \caller env responded funds_available dat wasm_state ->
      invoke wasm_state (CI.Update m caller env responded funds_available dat))
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_update " n
    ]
  , query_methods = M.fromList
    [ (m, \caller env arg wasm_state ->
        snd <$> invoke wasm_state (CI.Query m caller env arg))
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_query " n
    ]
  , callbacks = \cb env responded funds_available res refund wasm_state ->
    invoke wasm_state (CI.Callback cb env responded funds_available res refund)
  , pre_upgrade_method = \wasm_state caller env ->
        snd <$> invoke wasm_state (CI.PreUpgrade wasm_mod caller env)
  , post_upgrade_method = \cid caller env mem dat ->
        initializeUpgrade wasm_mod cid caller env mem dat
  }

-- | Turns a query function into an update function
asUpdate ::
  (EntityId -> CI.Env -> Blob -> QueryFunc) ->
  (EntityId -> CI.Env -> Responded -> Funds -> Blob -> UpdateFunc)
asUpdate f caller env (Responded responded) _funds_available dat wasm_state
  | responded = error "asUpdate: responded == True"
  | otherwise =
    (\res -> (wasm_state, ([], no_funds, Just res))) <$>
    f caller env dat wasm_state
