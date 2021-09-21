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
import Data.Word
import Data.STRef
import Control.Monad.Except
import Control.Monad.ST

import qualified Data.Primitive.ByteArray as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BI

type Size = Word64
type Address = Word64
type HostM s = ExceptT String (ST s)
type Memory s = STRef s (A.MutableByteArray s)

pageSize :: Size
pageSize = 65536

new :: HostM s (Memory s)
new = lift (A.newByteArray 0 >>= newSTRef)

memorySizeInBytes :: Memory s -> ST s Size
memorySizeInBytes mem = do
  nBytes <- readSTRef mem >>= A.getSizeofMutableByteArray
  return $ fromIntegral nBytes

memorySizeInPages :: Memory s -> ST s Size
memorySizeInPages mem = do
  nBytes <- memorySizeInBytes mem
  return $ fromIntegral $ nBytes `quot` (fromIntegral pageSize)

checkAccess :: Memory s -> Address -> Size -> HostM s ()
checkAccess mem offset len = do
  n <- lift $ memorySizeInBytes mem
  when ((fromIntegral offset :: Integer) + fromIntegral len > fromIntegral n) $
    throwError "stable memory error: out of bounds"

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
  oldNumBytes <- memorySizeInBytes mem
  oldNumPages <- memorySizeInPages mem
  arr <- readSTRef mem
  let extraSize = pageSize * delta
  let newNumBytes = oldNumBytes + extraSize
  newArr <- A.resizeMutableByteArray arr (fromIntegral newNumBytes)
  A.fillByteArray newArr (fromIntegral oldNumBytes) (fromIntegral extraSize) 0
  writeSTRef mem newArr
  return oldNumPages

read :: Memory s -> Address -> Size -> HostM s ByteString
read mem ptr len = do
  checkAccess mem ptr len
  lift $ toByteString mem ptr len

write :: Memory s -> Address -> ByteString -> HostM s ()
write mem ptr blob = do
  let n = fromIntegral $ BL.length blob
  checkAccess mem ptr n
  if n > 0
  then lift $ do
         dst <- readSTRef mem
         forM_ [0 .. n - 1] $ \idx ->
           A.writeByteArray dst (fromIntegral ptr + fromIntegral idx) $ BL.index blob (fromIntegral idx)
  else return ()

export :: Memory s -> ST s ByteString
export mem = do
  nBytes <- memorySizeInBytes mem
  toByteString mem 0 (fromIntegral nBytes)

imp :: Memory s -> ByteString -> ST s ()
imp mem src = do
  dst <- A.unsafeThawByteArray $ A.byteArrayFromListN (fromIntegral $ BL.length src) (BL.unpack src)
  writeSTRef mem dst

