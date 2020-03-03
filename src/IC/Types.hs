{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module IC.Types where

import IC.Debug.JSON
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Hex as T
import Data.Int

type (↦) = M.Map

-- Basic types

type Blob = BS.ByteString
newtype EntityId = EntityId { rawEntityId :: Blob }
    deriving (Show, Eq, Ord, Generic, ToJSON)

instance ToJSONKey EntityId where
  toJSONKey = contramapToJSONKeyFunction rawEntityId toJSONKey

type CanisterId = EntityId
type UserId = EntityId
type MethodName = String
type RequestID = Blob

prettyBlob :: Blob -> String
prettyBlob b = "0x" ++ T.unpack (T.encodeHex (BS.toStrict b))

prettyID :: EntityId -> String
prettyID = prettyBlob . rawEntityId -- implement the "ic:…" stuff

newtype Responded = Responded Bool
  deriving (Show, Generic, ToJSON)

data RejectCode
    = RC_SYS_FATAL
    | RC_SYS_TRANSIENT
    | RC_DESTINATION_INVALID
    | RC_CANISTER_REJECT
    | RC_CANISTER_ERROR
  deriving (Show, Generic, ToJSON)

rejectCode :: RejectCode -> Int
rejectCode RC_SYS_FATAL           = 1
rejectCode RC_SYS_TRANSIENT       = 2
rejectCode RC_DESTINATION_INVALID = 3
rejectCode RC_CANISTER_REJECT     = 4
rejectCode RC_CANISTER_ERROR      = 5


data Response = Reply Blob | Reject (RejectCode, String)
  deriving (Show, Generic, ToJSON)

-- Abstract canisters

data TrapOr a = Trap String | Return a deriving Functor

data WasmClosure = WasmClosure
  { closure_idx :: Int32
  , closure_env :: Int32
  }
  deriving (Show, Generic, ToJSON)

data Callback = Callback
  { reply_callback :: WasmClosure
  , reject_callback :: WasmClosure
  }
  deriving (Show, Generic, ToJSON)

data MethodCall = MethodCall
  { call_callee :: CanisterId
  , call_method_name :: MethodName
  , call_arg :: Blob
  , call_callback :: Callback
  }
  deriving (Show, Generic, ToJSON)

type ExistingCanisters = [CanisterId]

type InitResult = [MethodCall]
type UpdateResult = ([MethodCall], Maybe Response)

type StableMemory = Blob
