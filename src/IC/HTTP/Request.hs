{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{- | Parses/produces generic requests -}
module IC.HTTP.Request where

import Numeric.Natural
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Control.Monad.State
import Control.Monad.Except
import qualified Data.HashMap.Lazy as HM

import IC.Types
import IC.Ref (AsyncRequest(..), SyncRequest(..),
  RequestStatus(..), CompletionValue(..))
import IC.HTTP.GenR

dummyUserId :: EntityId
dummyUserId = EntityId $ BS.pack [0xCA, 0xFF, 0xEE]

-- Parsing requests to /submit
asyncRequest :: GenR -> Either T.Text AsyncRequest
asyncRequest = record $ do
    t <- field text "request_type"
    case t of
        "install_code" -> do
            cid <- EntityId <$> field blob "canister_id"
            -- FIXME: Sender should not be optional
            sender <- maybe dummyUserId EntityId <$> optionalField blob "sender"
            mod <- field blob "module"
            arg <- field blob "arg"
            _ <- field percentage "compute_allocation"
            _ <- field blob "nonce"
            return $ InstallRequest cid sender mod arg
        _ -> throwError $ "Unknown request type " <> t

-- Parsing requests to /response
syncRequest :: GenR -> Either T.Text SyncRequest
syncRequest = record $ do
    t <- field text "request_type"
    case t of
        "request_status" -> do
            rid <- field blob "request_id"
            return $ StatusRequest rid
        _ -> throwError $ "Unknown request type " <> t

-- Printing responses
response :: RequestStatus -> GenR
response = \case
    Unknown -> rec ["status" =: GText "unknown"]
    Received -> rec ["status" =: GText "received"]
    Processing -> rec ["status" =: GText "processing"]
    Rejected (c, s) -> rec
        [ "status" =: GText "rejected"
        , "reject_code" =: GNat (fromIntegral (rejectCode c))
        , "reject_message" =: GText (T.pack s)
        ]
    Completed CompleteUnit -> rec ["status" =: GText "replied"]
    Completed (CompleteCanisterId id) -> rec
        [ "status" =: GText "replied"
        , "reply" =: rec [ "canister_id" =: GBlob (BS.toStrict (rawEntityId id)) ]
        ]
    Completed (CompleteArg blob) -> rec
        [ "status" =: GText "replied"
        , "reply" =: rec [ "arg" =: GBlob (BS.toStrict blob) ]
        ]
  where
    -- Convenient syntax
    (=:) = HM.singleton
    rec = GRec . mconcat 


-- A monad to parse a record
-- (reading each field once, checking for left-over fields in the end)
type ParseM = Except T.Text
type RecordM = StateT (HM.HashMap T.Text GenR) (Except T.Text)
type Field a = GenR -> ParseM a

record :: RecordM a -> GenR -> Either T.Text a
record m (GRec hm) = runExcept $ (`evalStateT` hm) $ do
    x <- m
    -- Check for left-over fields
    hm <- get
    unless (HM.null hm) $
        throwError $ "Unexpected fields: " <> T.intercalate ", " (HM.keys hm)
    return x
record _ _ = Left "Expected CBOR record"


field :: (GenR -> ParseM a) -> T.Text -> RecordM a
field parse name = do
    hm <- get
    put (HM.delete name hm)
    case HM.lookup name hm of
        Nothing -> throwError $ "Missing expected field \"" <> name <> "\""
        Just gr -> lift $ parse gr

optionalField :: (GenR -> ParseM a) -> T.Text -> RecordM (Maybe a)
optionalField parse name = do
    hm <- get
    put (HM.delete name hm)
    case HM.lookup name hm of
        Nothing -> return Nothing
        Just gr -> lift $ Just <$> parse gr

text :: Field T.Text
text (GText t) = return t
text _ = throwError "Expected text value"

blob :: Field BS.ByteString
blob (GBlob b) = return (BS.fromStrict b)
blob _ = throwError "Expected blob"

nat :: Field Natural
nat (GNat n) = return n
nat _ = throwError "Expected natural number"

percentage :: Field Natural
percentage gr = do
    n <- nat gr
    unless (0 <= n && n <= 100) $
        throwError "Expected a percentage (0..100)"
    return n



