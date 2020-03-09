{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module IC.HTTP.WAI where

import Network.Wai
import Control.Concurrent.MVar
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.State
import Data.Aeson as JSON

import IC.Ref
import IC.HTTP.GenR
import IC.HTTP.Status
import IC.HTTP.CBOR
import IC.HTTP.Request
import IC.HTTP.RequestId
import IC.Debug.JSON ()

startApp :: IO Application
startApp = do
    stateVar <- newMVar [initialIC]
    return $ handle stateVar

handle :: MVar [IC] -> Application
handle stateVar req respond = case (requestMethod req, pathInfo req) of
    ("GET", []) -> withHistory $ json status200
    ("GET", ["api","v1","status"]) ->
        cbor status200 IC.HTTP.Status.r
    ("POST", ["api","v1","submit"]) ->
        withSignedCBOR $ \gr -> case  asyncRequest gr of
            Left err -> invalidRequest err
            Right ar -> do
                runIC $ submitRequest (requestId gr) ar
                loopIC runStep
                cbor status200 emptyR
    ("POST", ["api","v1","read"]) ->
        withSignedCBOR $ \gr -> case syncRequest gr of
            Left err -> invalidRequest err
            Right ar -> do
                r <- peekIC $ readRequest ar
                cbor status200 (IC.HTTP.Request.response r)
    _ -> notFound
  where
    runIC :: StateT IC IO a -> IO a
    runIC a = modifyMVar stateVar $ \(s:ss) -> do
        (x, s') <- runStateT a s
        return (s':s:ss, x)

    peekIC :: StateT IC IO a -> IO a
    peekIC a = modifyMVar stateVar $ \(s:ss) -> do
        (x, _s') <- runStateT a s
        return (s:ss, x)

    stepIC :: StateT IC IO Bool -> IO Bool
    stepIC a = modifyMVar stateVar $ \(s:ss) -> do
        (changed, s') <- runStateT a s
        return (if changed then s':s:ss else s:ss, changed)

    loopIC :: StateT IC IO Bool -> IO ()
    loopIC a = stepIC a >>= \case
        True -> loopIC a
        False -> return ()

    withHistory :: ([IC] -> IO a) -> IO a
    withHistory a = readMVar stateVar >>= a . reverse

    cbor status gr = respond $ responseBuilder
        status
        [ (hContentType, "application/cbor") ]
        (IC.HTTP.CBOR.encode gr)

    json status x = respond $ responseBuilder
        status
        [ (hContentType, "application/json") ]
        (JSON.fromEncoding $ JSON.toEncoding x)

    invalidRequest msg = do
        print (T.unpack msg)
        respond $ responseBuilder status400 [] (T.encodeUtf8Builder msg)
    notFound = respond $ responseLBS status404 [] "Not found\n"

    withCBOR k = case lookup hContentType (requestHeaders req) of
        Just "application/cbor" -> do
            body <- strictRequestBody req
            case IC.HTTP.CBOR.decode body of
                Left err -> invalidRequest err
                Right gr -> k gr
        _ -> invalidRequest "Expected application/cbor request"

    withSignedCBOR k = withCBOR $ \gr -> case stripEnvelope gr of
        Left err -> invalidRequest err
        Right content -> k content

