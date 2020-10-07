{-# LANGUAGE GADTs #-}

{-|
This module defines the various entry points provided by "IC.Canister.Imp" as a data type describing the arguments and results.

The point of this abstraction is to to allow adding new entry points, or changing the arguments or result values, without changing the modules "IC.Canister.Pure" or "IC.Canister.Persisted", that sit between "IC.Canister.Imp" and "IC.Canister".

-}
module IC.Canister.Interface where

import IC.Types
import IC.Funds
import IC.Wasm.Winter (Module)

-- | This data type contains all read-only data that should be available to the
-- canister almost always
data Env = Env
    { time :: Timestamp
    , balance :: Funds
    }

data CanisterMethod r where
    Initialize :: Module -> EntityId -> Env -> Blob -> CanisterMethod ()
    Query :: MethodName -> EntityId -> Env -> Blob -> CanisterMethod Response
    Update :: MethodName -> EntityId -> Env -> Responded -> Funds -> Blob -> CanisterMethod UpdateResult
    Callback :: Callback -> Env -> Responded -> Funds -> Response -> Funds -> CanisterMethod UpdateResult
    PreUpgrade :: Module -> EntityId -> Env -> CanisterMethod Blob
    PostUpgrade :: Module -> EntityId -> Env -> Blob -> Blob -> CanisterMethod ()
