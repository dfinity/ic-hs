{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
This module provides a way to persist the state of a Winter Wasm instance, and
to recover it.

It is tailored to the use by ic-ref. For example it assumes that the
table of a wasm instance is immutable.
-}

module IC.Wasm.Wasmtime.Persist
  ( PInstance(..)
  , Persistable(..)
  , persistInstance
  , resumeInstance
  )
  where

import Control.Monad
import Control.Monad.ST
import Data.Primitive.MutVar
import qualified Data.IntMap as IM
import qualified Data.Map.Lazy as M
import qualified Data.Vector as V
import Data.ByteString.Lazy (ByteString)
import Data.Kind (Type)

import qualified IC.Canister.StableMemory as Stable

import qualified Wasm.Runtime.Global as W
import qualified Wasm.Runtime.Instance as W
import qualified Wasm.Runtime.Memory as W
import qualified Wasm.Syntax.Values as W
import qualified Wasm.Util.Source as W

import IC.Wasm.Wasmtime (Instance)

-- |
-- This stores data read from an instance.
newtype PInstance = PInstance (Persisted (Instance RealWorld))
  deriving Show

persistInstance :: Instance RealWorld -> IO PInstance
persistInstance i = PInstance <$> persist i

resumeInstance :: Instance RealWorld -> PInstance -> IO ()
resumeInstance i (PInstance p) = resume i p

class Monad (M a) => Persistable a where
  type Persisted a :: Type
  type M a :: Type -> Type
  persist :: a -> M a (Persisted a)
  resume :: a -> Persisted a -> M a ()

instance Persistable (Stable.Memory) where
  type Persisted (Stable.Memory) = Stable.Repr
  type M (Stable.Memory) = IO
  persist = Stable.export
  resume = Stable.imp

instance Persistable (Instance RealWorld) where
  type Persisted (Instance RealWorld) = ()
  type M (Instance RealWorld) = IO
  persist i = return ()
  resume = error "not implemented"

instance Persistable a => Persistable [a] where
  type Persisted [a] = [Persisted a]
  type M [a] = M a
  persist = mapM persist
  resume xs ys = do
    unless (length xs == length ys) $ error "Lengths don't match"
    zipWithM_ resume xs ys

instance Persistable a => Persistable (V.Vector a) where
  type Persisted (V.Vector a) = V.Vector (Persisted a)
  type M (V.Vector a) = M a
  persist = mapM persist
  resume xs ys = do
    unless (V.length xs == V.length ys) $ error "Lengths don't match"
    V.zipWithM_ resume xs ys

instance (Eq k, Persistable a) => Persistable (M.Map k a) where
  type Persisted (M.Map k a) = M.Map k (Persisted a)
  type M (M.Map k a) = M a
  persist = mapM persist
  resume xs ys = do
    unless (M.keys xs == M.keys ys) $ error "Map keys don't match"
    zipWithM_ resume (M.elems xs) (M.elems ys)

instance Persistable a => Persistable (IM.IntMap a) where
  type Persisted (IM.IntMap a) = M.Map Int (Persisted a)
  type M (IM.IntMap a) = M a
  persist = mapM persist . M.fromList . IM.toList
  resume xs ys = do
    let ys' = IM.fromList (M.toList ys)
    unless (IM.keys xs == IM.keys ys') $ error "Map keys don't match"
    zipWithM_ resume (IM.elems xs) (IM.elems ys')

instance Persistable a => Persistable (a, Int) where
  type Persisted (a, Int) = Persisted a
  type M (a, Int) = M a
  persist (a, _i) = persist a
  resume (a, _i) p = resume a p
