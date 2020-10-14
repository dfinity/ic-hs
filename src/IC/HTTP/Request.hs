{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{- | Parses/produces generic requests -}
module IC.HTTP.Request where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HM
import Control.Monad.Except
import Data.Time.Clock.POSIX

import IC.Types
import IC.Crypto
import IC.Ref (AsyncRequest(..), SyncRequest(..),
  RequestStatus(..), CompletionValue(..))
import IC.HTTP.RequestId
import IC.HTTP.GenR
import IC.HTTP.GenR.Parse

dummyUserId :: EntityId
dummyUserId = EntityId $ BS.pack [0xCA, 0xFF, 0xEE]

stripEnvelope :: GenR -> Either T.Text (Maybe PublicKey, GenR)
stripEnvelope = record $ do
    content <- field anyType "content"
    pk <- optionalField blob "sender_pubkey"
    sig <- optionalField blob "sender_sig"
    case (pk, sig) of
        (Just pk, Just sig) -> do
            lift $ verify "\x0Aic-request" pk (requestId content) sig
            return (Just pk, content)
        (Nothing, Nothing) -> return (Nothing, content)
        _ -> throwError "Need to set either both or none of sender_pubkey and sender_sig"

getTimestamp :: IO Timestamp
getTimestamp = do
    t <- getPOSIXTime
    return $ Timestamp $ round (t * 1000_000_000)

checkExpiry :: GenR -> IO (Either T.Text ())
checkExpiry (GRec hm)
    | Just (GNat expiry) <- HM.lookup "ingress_expiry" hm = runExceptT $ do
        -- Here we check that the expiry field is not in the past and not
        -- too far in the future
        Timestamp t <- lift getTimestamp
        unless (expiry > t) $
            throwError $ "Expiry is " <> T.pack (show ((t - expiry)`div`1000_000_000)) <> " seconds in the past"
        unless (expiry < t + max_future) $
            throwError $ "Expiry is " <> T.pack (show ((expiry - t)`div`1000_000_000)) <> " seconds in the future"
  where
    -- max expiry time is 5 minutes
    max_future = 5*60*1000_000_000
checkExpiry _ = runExceptT (throwError "No ingress_expiry field found")

-- Parsing requests to /submit
asyncRequest :: GenR -> Either T.Text AsyncRequest
asyncRequest = record $ do
    t <- field text "request_type"
    _ <- optionalField blob "nonce"
    e <- Timestamp <$> field nat "ingress_expiry"
    case t of
        "call" -> do
            cid <- EntityId <$> field blob "canister_id"
            sender <- EntityId <$> field blob "sender"
            method_name <- field text "method_name"
            arg <- field blob "arg"
            return $ UpdateRequest e cid sender (T.unpack method_name) arg
        _ -> throwError $ "Unknown request type \"" <> t <> "\""

-- Parsing requests to /response
syncRequest :: GenR -> Either T.Text SyncRequest
syncRequest = record $ do
    t <- field text "request_type"
    _ <- optionalField blob "nonce"
    e <- Timestamp <$> field nat "ingress_expiry"
    case t of
        "request_status" -> do
            rid <- field blob "request_id"
            return $ StatusRequest e rid
        "query" -> do
            cid <- EntityId <$> field blob "canister_id"
            sender <- EntityId <$> field blob "sender"
            method_name <- field text "method_name"
            arg <- field blob "arg"
            return $ QueryRequest e cid sender (T.unpack method_name) arg
        _ -> throwError $ "Unknown request type \"" <> t <> "\""

-- Printing responses
response :: Maybe Timestamp -> RequestStatus -> GenR
response mt = \case
    Unknown -> statusRec "unknown" []
    Received -> statusRec "received" []
    Processing -> statusRec "processing" []
    Rejected (c, s) -> statusRec "rejected"
        [ "reject_code" =: GNat (fromIntegral (rejectCode c))
        , "reject_message" =: GText (T.pack s)
        ]
    Completed r -> statusRec "replied"
        [ "reply" =: case r of
            CompleteUnit ->
                rec []
            CompleteCanisterId id ->
                rec [ "canister_id" =: GBlob (rawEntityId id) ]
            CompleteArg blob ->
                rec [ "arg" =: GBlob blob ]
        ]
  where
    statusRec s fs = rec $
        [ "status" =: GText s ] <>
        [ "time" =: GNat t | Just (Timestamp t) <- pure mt ] <>
        fs
