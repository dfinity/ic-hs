{-# LANGUAGE ScopedTypeVariables #-}
{-|
This module provides a wrapper around primitive byte array, exposing just
the bits needed for accessing the stable memory.
-}
module IC.Canister.StableMemory
  ( Memory
  , Repr
  , new
  , size
  , grow
  , read
  , write
  , export
  , imp
  , serialize
  , deserialize
  )
where

import Prelude hiding (read)

import Data.ByteString.Lazy (ByteString)
import Data.Word
import Data.IORef
import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Control.Monad.Except
import Control.Monad.ST

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as BI

type Size = Word64
type Address = Word64

type HostM = ExceptT String IO
type Memory = IORef Repr

-- | Size of a WebAssembly page.
pageSize :: Size
pageSize = 65536

-- NOTE: This stable memory representation is optimized for sparse writes and
-- arbitrary resizes.
--
-- Resizes and writes are very cheap, but the cost of reads is propotional to
-- the size of the write history. This enables us to work with large memories
-- without allocating gigabytes of storage. The main motivation for this
-- representation was testing 64 bit stable memory API and its interaction with
-- 32 bit API.
--
-- See https://github.com/dfinity/ic-hs/issues/28 for more details.

-- | Immutable representation of stable memory that can be used for snapshotting.
data Repr
  = Repr
  { smSize :: Size
  , smWrites :: [(Address, BS.ByteString)]
  } deriving (Show)

-- | Constructs a new empty memory.
new :: IO (Memory)
new = newIORef $ Repr 0 []

memorySizeInBytes :: Memory -> IO Size
memorySizeInBytes mem = (pageSize *) <$> memorySizeInPages mem

memorySizeInPages :: Memory -> IO Size
memorySizeInPages mem = smSize <$> readIORef mem

-- | Checks that range [offset, offset + len] lies within the memory.
checkAccess :: Memory -> Address -> Size -> HostM ()
checkAccess mem offset len = do
  n <- lift $ memorySizeInBytes mem
  when ((fromIntegral offset :: Integer) + fromIntegral len > fromIntegral n) $
    throwError "stable memory error: out of bounds"

-- | Extracts a bytestring from stable memory representation.
toByteString :: Repr -> Address -> Size -> ByteString
toByteString repr offset len = BL.fromStrict $ BI.unsafeCreate (fromIntegral len) $ \dst -> do
  _ <- BI.memset dst 0 (fromIntegral len)
  forM_ (reverse $ smWrites repr) $ \(addr, blob) -> do
    let n = fromIntegral $ BS.length blob
    let copyStart = max offset addr
    let copyEnd = min (offset + len) (addr + n)
    when (copyStart < copyEnd) $ do
      let (fptr, blobOffset, _len) = BI.toForeignPtr blob
      withForeignPtr fptr $ \src ->
        BI.memcpy (plusPtr dst $ fromIntegral $ copyStart - offset)
                  (plusPtr src $ blobOffset + (fromIntegral $ copyStart - addr))
                  (fromIntegral $ copyEnd - copyStart)

-- | Returns the size of stable memory in WebAssembly pages.
size :: Memory -> IO Size
size = memorySizeInPages

-- | Attempts to grow stable memory by @delta@ pages.
grow :: Memory -> Size -> IO Size
grow mem delta = do
  repr <- readIORef mem
  let oldSize = smSize repr
  writeIORef mem (repr { smSize = oldSize + delta })
  return oldSize

-- | Reads a range of bytes from memory.
read :: Memory -> Address -> Size -> HostM ByteString
read mem ptr len = do
  checkAccess mem ptr len
  do
    repr <- lift $ readIORef mem
    return $ toByteString repr ptr len

-- | Writes a byte string at the specified offset.
write :: Memory -> Address -> ByteString -> HostM ()
write mem ptr blob = do
  checkAccess mem ptr (fromIntegral $ BL.length blob)
  lift $ modifyIORef' mem (\repr -> repr { smWrites = (ptr, BL.toStrict blob) : smWrites repr })

-- | Exports immutable memory representation.
export :: Memory -> IO Repr
export = readIORef

-- | Sets the contents of memory to a previously exported value.
imp :: Memory -> Repr -> IO ()
imp = writeIORef

-- | Converts internal memory representation into a bytestring.
serialize :: Repr -> ByteString
serialize repr = toByteString repr 0 (pageSize * smSize repr)

-- | Constructs internal memory representation from a @blob@.
-- Throws an exception if the length of the @blob@ is not a multiple of 64KiB.
deserialize :: ByteString -> Repr
deserialize blob =
  let n = fromIntegral $ BL.length blob
  in if (n `mod` pageSize) /= 0
     then error "StableMemory.deserialize: blob size is not a multiple of 64KiB"
     else Repr (n `quot` pageSize) [(0, BL.toStrict blob)]
