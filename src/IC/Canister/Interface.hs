{-# LANGUAGE GADTs #-}

{-|
This module defines the various entry points provided by "IC.Canister.Imp" as a data type describing the arguments and results.

The point of this abstraction is to to allow adding new entry points, or changing the arguments or result values, without changing the modules "IC.Canister.Pure" or "IC.Canister.Persisted", that sit between "IC.Canister.Imp" and "IC.Canister".

-}
module IC.Canister.Interface where

import IC.Types
import IC.Wasm.Winter (Module)


data CanisterMethod r where
    Initialize :: Module -> EntityId -> Timestamp -> Blob -> CanisterMethod ()
    Query :: MethodName -> EntityId -> Timestamp -> Blob -> CanisterMethod Response
    Update :: MethodName -> EntityId -> Timestamp -> Responded -> Blob -> CanisterMethod UpdateResult
    Callback :: Callback -> Timestamp -> Responded -> Response -> CanisterMethod UpdateResult
    PreUpgrade :: Module -> EntityId -> Timestamp -> CanisterMethod Blob
    PostUpgrade :: Module -> EntityId -> Timestamp -> Blob -> Blob -> CanisterMethod ()
