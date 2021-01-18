{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IC.HTTP where

import Network.Wai
import Control.Concurrent (forkIO)
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.State
import Control.Monad.Except
import Data.Aeson as JSON

import IC.Types
import IC.Ref
import IC.HTTP.Status
import IC.HTTP.CBOR
import IC.HTTP.GenR
import IC.HTTP.Request
import IC.HTTP.RequestId
import IC.Debug.JSON ()
import IC.Serialise ()
import IC.StateFile

withApp :: Maybe FilePath -> (Application -> IO a) -> IO a
withApp backingFile action =
    withStore initialIC backingFile (action . handle)

handle :: Store IC -> Application
handle store req respond = case (requestMethod req, pathInfo req) of
    ("GET", []) -> peekStore store >>= json status200
    ("GET", ["api","v1","status"]) -> do
        r <- peekIC $ gets IC.HTTP.Status.r
        cbor status200 r
    ("POST", ["api","v1","submit"]) ->
        withSignedCBOR $ \(gr, ev) -> case asyncRequest gr of
            Left err -> invalidRequest err
            Right ar -> runIC $ do
                t <- lift getTimestamp
                runExceptT (authAsyncRequest t ev ar) >>= \case
                    Left err ->
                        lift $ invalidRequest err
                    Right () -> do
                        submitRequest (requestId gr) ar
                        lift $ empty status202
    ("POST", ["api","v1","read"]) ->
        withSignedCBOR $ \(gr, ev) -> case syncRequest gr of
            Left err -> invalidRequest err
            Right sr -> peekIC $ do
                t <- lift getTimestamp
                runExceptT (authSyncRequest t ev sr) >>= \case
                    Left err ->
                        lift $ invalidRequest err
                    Right () -> do
                        t <- lift getTimestamp
                        r <- readRequest t sr
                        lift $ cbor status200 (IC.HTTP.Request.response r)
    _ -> notFound
  where
    runIC :: StateT IC IO a -> IO a
    runIC a = do
      x <- modifyStore store a
      -- begin processing in the background (it is important that
      -- this thread returns, else warp is blocked somehow)
      void $ forkIO loopIC
      return x

    -- Not atomic, reads most recent state
    peekIC :: StateT IC IO a -> IO a
    peekIC a = peekStore store >>= evalStateT a

    loopIC :: IO ()
    loopIC = modifyStore store runStep >>= \case
        True -> loopIC
        False -> return ()

    cbor status gr = respond $ responseBuilder
        status
        [ (hContentType, "application/cbor") ]
        (IC.HTTP.CBOR.encode gr)

    json status x = respond $ responseBuilder
        status
        [ (hContentType, "application/json") ]
        (JSON.fromEncoding $ JSON.toEncoding x)

    plain status x = respond $ responseBuilder
        status
        [ (hContentType, "text/plain") ]
        x

    empty status = plain status mempty

    invalidRequest msg = do
        when False $ print (T.unpack msg)
        -- ^ When testing against dfx, and until it prints error messages
        -- this can be enabled
        plain status400 (T.encodeUtf8Builder msg)
    notFound = plain status404 "Not found\n"


    withCBOR k = case lookup hContentType (requestHeaders req) of
        Just "application/cbor" -> do
            body <- strictRequestBody req
            case IC.HTTP.CBOR.decode body of
                Left err -> invalidRequest err
                Right gr -> k gr
        _ -> invalidRequest "Expected application/cbor request"

    withSignedCBOR :: ((GenR, EnvValidity) -> IO ResponseReceived) -> IO ResponseReceived
    withSignedCBOR k = withCBOR $ either invalidRequest k . stripEnvelope
