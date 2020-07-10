{-# LANGUAGE TypeOperators #-}

{-|
A “pure” implementation of canisters, using "IC.Canister.Imp", but just replaying when needed.
Note that this is quadratic in the number of update calls, so do not run such canisters for long.

We could do some hacking caching of state using stable names, so that, as long as no trap occurs, 'replay' is fast.
-}

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
import IC.Wasm.Winter (parseModule, exportedFunctions, Module)

import qualified IC.Canister.Interface as CI
-- Here we can swap out the persistence implementation
import IC.Canister.Persisted

type InitFunc = CanisterId -> EntityId -> Timestamp -> Blob -> TrapOr WasmState
type UpdateFunc = WasmState -> TrapOr (WasmState, UpdateResult)
type QueryFunc = WasmState -> TrapOr Response

data CanisterModule = CanisterModule
  { init_method :: InitFunc
  , update_methods :: MethodName ↦ (EntityId -> Timestamp -> Responded -> Blob -> UpdateFunc)
  , query_methods :: MethodName ↦ (EntityId -> Timestamp -> Blob -> QueryFunc)
  , callbacks :: Callback -> Timestamp -> Responded -> Response -> UpdateFunc
  , pre_upgrade_method :: WasmState -> EntityId -> Timestamp -> TrapOr Blob
  , post_upgrade_method :: CanisterId -> EntityId -> Timestamp -> Blob -> Blob -> TrapOr WasmState
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
  { init_method = \cid caller time dat -> initialize wasm_mod cid caller time dat
  , update_methods = M.fromList
    [ (m,
      \caller time responded dat wasm_state ->
      invoke wasm_state (CI.Update m caller time responded dat))
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_update " n
    ]
  , query_methods = M.fromList
    [ (m, \caller time arg wasm_state ->
        snd <$> invoke wasm_state (CI.Query m caller time arg))
    | n <- exportedFunctions wasm_mod
    , Just m <- return $ stripPrefix "canister_query " n
    ]
  , callbacks = \cb time responded res wasm_state ->
    invoke wasm_state (CI.Callback cb time responded res)
  , pre_upgrade_method = \wasm_state caller time ->
        snd <$> invoke wasm_state (CI.PreUpgrade wasm_mod caller time)
  , post_upgrade_method = \cid caller time mem dat ->
        initializeUpgrade wasm_mod cid caller time mem dat
  }

-- | Turns a query function into an update function
asUpdate ::
  (EntityId -> Timestamp -> Blob -> QueryFunc) ->
  (EntityId -> Timestamp -> Responded -> Blob -> UpdateFunc)
asUpdate f caller time (Responded responded) dat wasm_state
  | responded = error "asUpdate: responded == True"
  | otherwise =
    (\res -> (wasm_state, ([], Just res))) <$>
    f caller time dat wasm_state
