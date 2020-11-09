{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IC.HTTP where

import Network.Wai
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Data.IORef
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

startApp :: IO Application
startApp = do
    ic <- initialIC
    stateVar <- newMVar ic
    lastState <- newIORef ic
    return $ handle stateVar lastState

handle :: MVar IC -> IORef IC -> Application
handle stateVar lastState req respond = case (requestMethod req, pathInfo req) of
    ("GET", []) -> readIORef lastState >>= json status200
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
    -- This modifies state, so must be atomic, so blocks on stateVar
    runIC :: StateT IC IO a -> IO a
    runIC a = do
      x <- modifyMVar stateVar $ \s -> do
        (x, s') <- runStateT a s
        writeIORef lastState s'
        return (s', x)
      -- begin processing in the background (it is important that
      -- this thread returns, else warp is blocked somehow)
      void $ forkIO (loopIC runStep)
      return x

    -- Not atomic, reads most recent state
    peekIC :: StateT IC IO a -> IO a
    peekIC a = readIORef lastState >>= evalStateT a

    -- This modifies state, so must be atomic, so blocks on stateVar
    stepIC :: StateT IC IO Bool -> IO Bool
    stepIC a = modifyMVar stateVar $ \s -> do
        (changed, s') <- runStateT a s
        when changed $ writeIORef lastState s'
        return (if changed then s' else s, changed)

    loopIC :: StateT IC IO Bool -> IO ()
    loopIC a = stepIC a >>= \case
        True -> loopIC a
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
