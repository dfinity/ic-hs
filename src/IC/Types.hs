{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module IC.Types where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Hex as T
import Data.Digest.CRC
import Data.Digest.CRC32
import Data.ByteString.Base32
import Data.Int
import Data.Word
import Data.List
import Data.List.Split (chunksOf)

type (↦) = M.Map

-- Basic types

type Blob = BS.ByteString
type PublicKey = Blob
newtype EntityId = EntityId { rawEntityId :: Blob }
    deriving (Show, Eq, Ord)

type CanisterId = EntityId
type UserId = EntityId
type MethodName = String
type RequestID = Blob

prettyBlob :: Blob -> String
prettyBlob b = "0x" ++ T.unpack (T.encodeHex (BS.toStrict b))

prettyID :: EntityId -> String
prettyID (EntityId blob) =
    intercalate "-" (chunksOf 5 (base32 (checkbytes <> blob)))
  where
    CRC32 checksum = digest (BS.toStrict blob)
    checkbytes = BS.toLazyByteString (BS.word32BE checksum)

    base32 = filter (/='=') . T.unpack . T.toLower . encodeBase32 . BS.toStrict


newtype Responded = Responded Bool
  deriving Show

newtype Timestamp = Timestamp Word64
  deriving (Show, Num)

data RejectCode
    = RC_SYS_FATAL
    | RC_SYS_TRANSIENT
    | RC_DESTINATION_INVALID
    | RC_CANISTER_REJECT
    | RC_CANISTER_ERROR
  deriving Show

rejectCode :: RejectCode -> Int
rejectCode RC_SYS_FATAL           = 1
rejectCode RC_SYS_TRANSIENT       = 2
rejectCode RC_DESTINATION_INVALID = 3
rejectCode RC_CANISTER_REJECT     = 4
rejectCode RC_CANISTER_ERROR      = 5


data Response = Reply Blob | Reject (RejectCode, String)
  deriving Show

-- Abstract canisters

data TrapOr a = Trap String | Return a deriving Functor

data WasmClosure = WasmClosure
  { closure_idx :: Int32
  , closure_env :: Int32
  }
  deriving Show

data Callback = Callback
  { reply_callback :: WasmClosure
  , reject_callback :: WasmClosure
  }
  deriving Show

data MethodCall = MethodCall
  { call_callee :: CanisterId
  , call_method_name :: MethodName
  , call_arg :: Blob
  , call_callback :: Callback
  }
  deriving Show

type ExistingCanisters = [CanisterId]

type UpdateResult = ([MethodCall], Maybe Response)

type StableMemory = Blob
