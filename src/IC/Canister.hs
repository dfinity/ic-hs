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

type InitFunc = CanisterId -> EntityId -> Timestamp -> Funds -> Blob -> TrapOr WasmState
type UpdateFunc = WasmState -> TrapOr (WasmState, UpdateResult)
type QueryFunc = WasmState -> TrapOr Response

data CanisterModule = CanisterModule
  { init_method :: InitFunc
  , update_methods :: MethodName ↦ (EntityId -> Timestamp -> Funds -> Responded -> Funds -> Blob -> UpdateFunc)
  , query_methods :: MethodName ↦ (EntityId -> Timestamp -> Funds -> Blob -> QueryFunc)
  , callbacks :: Callback -> Timestamp -> Funds -> Responded -> Funds -> Response -> Funds -> UpdateFunc
  , pre_upgrade_method :: WasmState -> EntityId -> Timestamp -> Funds -> TrapOr Blob
  , post_upgrade_method :: CanisterId -> EntityId -> Timestamp -> Funds -> Blob -> Blob -> TrapOr WasmState
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
  { init_method = \cid caller time balance dat -> initialize wasm_mod cid caller time balance dat
  , update_methods = M.fromList
    [ (m,
      \caller time balance responded funds_available dat wasm_state ->
      invoke wasm_state (CI.Update m caller time balance responded funds_available dat))
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_update " n
    ]
  , query_methods = M.fromList
    [ (m, \caller time balance arg wasm_state ->
        snd <$> invoke wasm_state (CI.Query m caller time balance arg))
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_query " n
    ]
  , callbacks = \cb time balance responded funds_available res refund wasm_state ->
    invoke wasm_state (CI.Callback cb time balance responded funds_available res refund)
  , pre_upgrade_method = \wasm_state caller time balance ->
        snd <$> invoke wasm_state (CI.PreUpgrade wasm_mod caller time balance)
  , post_upgrade_method = \cid caller time balance mem dat ->
        initializeUpgrade wasm_mod cid caller time balance mem dat
  }

-- | Turns a query function into an update function
asUpdate ::
  (EntityId -> Timestamp -> Funds -> Blob -> QueryFunc) ->
  (EntityId -> Timestamp -> Funds -> Responded -> Funds -> Blob -> UpdateFunc)
asUpdate f caller time balance (Responded responded) _funds_available dat wasm_state
  | responded = error "asUpdate: responded == True"
  | otherwise =
    (\res -> (wasm_state, ([], no_funds, Just res))) <$>
    f caller time balance dat wasm_state
