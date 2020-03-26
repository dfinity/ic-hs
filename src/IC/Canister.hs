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
--import IC.Wasm.Winter (parseModule, exportedFunctions, Module)

-- Here we can swap out the persistence implementation
-- import qualified IC.Canister.Interface as CI
-- import IC.Canister.Persisted

data WasmState = WasmState deriving Show

type InitFunc = CanisterId -> EntityId -> Blob -> TrapOr WasmState
type UpdateFunc = WasmState -> TrapOr (WasmState, UpdateResult)
type QueryFunc = WasmState -> TrapOr Response

data CanisterModule = CanisterModule
  { init_method :: InitFunc
  , update_methods :: MethodName ↦ (EntityId -> Responded -> Blob -> UpdateFunc)
  , query_methods :: MethodName ↦ (EntityId -> Blob -> QueryFunc)
  , callbacks :: Callback -> Responded -> Response -> UpdateFunc
  , pre_upgrade_method :: WasmState -> EntityId -> TrapOr Blob
  , post_upgrade_method :: CanisterId -> EntityId -> Blob -> Blob -> TrapOr WasmState
  }

instance Show CanisterModule where
    show _ = "CanisterModule{...}"

parseCanister :: Blob -> Either String CanisterModule
parseCanister bytes = undefined

-- | Turns a query function into an update function
asUpdate ::
  (EntityId -> Blob -> QueryFunc) ->
  (EntityId -> Responded -> Blob -> UpdateFunc)
asUpdate f caller (Responded responded) dat wasm_state
  | responded = error "asUpdate: responded == True"
  | otherwise =
    (\res -> (wasm_state, ([], Just res))) <$>
    f caller dat wasm_state
