{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module IC.Canister.Snapshot ( CanisterSnapshot(..) ) where

import qualified Data.ByteString.Lazy as BS

import IC.Types
import IC.Wasm.Wasmtime (Module)
import IC.Wasm.Wasmtime.Persist
import IC.Canister.Imp
import IC.Canister.StableMemory as Stable
import IC.Purify

data CanisterSnapshot = CanisterSnapshot
    { wsModule :: BS.ByteString
    , wsInstances :: PInstance
    , wsStableMem :: Stable.Repr
    } deriving Show

{-
instance Persistable ImpState where
  type Persisted (ImpState) = CanisterSnapshot
  type M (ImpState) = IO
  persist (ImpState _ inst sm mod) = CanisterSnapshot mod <$> persistInstance inst <*> Stable.export sm
  resume _ (CanisterSnapshot wasm_mod pinst pmem) = do
        rs <- rawInstantiate wasm_mod >>= trapToFail
        resumeInstance (isInstance rs) pinst
        Stable.imp (isStableMem rs) pmem
        return rs
      where
        trapToFail (Trap err) = fail $ "replay failed: " ++ show err
        trapToFail (Return x) = return x
-}
