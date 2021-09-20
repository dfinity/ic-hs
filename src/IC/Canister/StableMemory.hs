{-# LANGUAGE ScopedTypeVariables #-}
{-|
This module provides a wrapper around primitive byte array, exposing just
the bits needed for accessing the stable memory.
-}
module IC.Canister.StableMemory
  ( Memory
  , new
  , size
  , grow
  , read
  , write
  , export
  , imp
  )
where

import Prelude hiding (read)

import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.STRef
import Control.Monad.Except
import Control.Monad.ST

import qualified Data.Primitive.ByteArray as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BI

type Size = Int32
type Address = Int64
type HostM s = ExceptT String (ST s)
type Memory s = STRef s (A.MutableByteArray s)

pageSize :: Int32
pageSize = 65536

new :: HostM s (Memory s)
new = lift (A.newByteArray 0 >>= newSTRef)

memorySizeInPages :: Memory s -> (ST s Size)
memorySizeInPages mem = do
  nBytes <- readSTRef mem >>= A.getSizeofMutableByteArray
  return $ fromIntegral $ nBytes `quot` (fromIntegral pageSize)

toByteString :: Memory s -> Address -> Size -> ST s ByteString
toByteString mem offset len = do
  tmp <- A.newPinnedByteArray (fromIntegral len)
  src <- readSTRef mem
  A.copyMutableByteArray tmp 0 src (fromIntegral offset) (fromIntegral len)
  let srcPtr = A.mutableByteArrayContents tmp
  return $ BL.fromStrict $ BI.unsafeCreate (fromIntegral len) $ \dstPtr ->
    BI.memcpy dstPtr srcPtr (fromIntegral len)

size :: Memory s -> HostM s Size
size = lift . memorySizeInPages

grow :: Memory s -> Size -> HostM s Size
grow mem delta = lift $ do
  nPages <- memorySizeInPages mem
  arr <- readSTRef mem
  newArr <- A.resizeMutableByteArray arr (fromIntegral $ pageSize * (delta + nPages))
  writeSTRef mem newArr
  return nPages

read :: Memory s -> Address -> Size -> HostM s ByteString
read mem ptr len = lift $ toByteString mem ptr len

write :: Memory s -> Address -> ByteString -> HostM s ()
write mem ptr blob = lift $ do
  dst <- readSTRef mem
  forM_ [ptr .. ptr + BL.length blob] $ \idx -> do
    A.writeByteArray dst (fromIntegral idx) (BL.index blob idx)

export :: Memory s -> ST s ByteString
export mem = do
  nBytes <- readSTRef mem >>= A.getSizeofMutableByteArray 
  toByteString mem 0 (fromIntegral nBytes)

imp :: Memory s -> ByteString -> ST s ()
imp mem src = do
  dst <- A.unsafeThawByteArray $ A.byteArrayFromListN (fromIntegral $ BL.length src) (BL.unpack src)
  writeSTRef mem dst

