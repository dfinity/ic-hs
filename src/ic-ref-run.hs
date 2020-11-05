{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Options.Applicative hiding (empty)
import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.Trans
import Control.Monad.Trans.State
import Text.Printf
import Data.List
import Data.IORef
import Data.Text.Prettyprint.Doc (pretty)
import Data.Time.Clock.POSIX

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Row (empty, (.==), (.+), type (.!), Label)
import qualified Codec.Candid as Candid
import qualified Data.Row.Variants as V


import IC.Version
import IC.Types
import IC.Ref
import IC.DRun.Parse (Ingress(..), parseFile)
import IC.Management


type DRun = StateT IC IO

dummyExpiry :: Timestamp
dummyExpiry = Timestamp 0

dummyUserId :: CanisterId
dummyUserId = EntityId $ B.pack [0xCA, 0xFF, 0xEE]

dummyRequestId :: AsyncRequest -> RequestID
dummyRequestId = B.fromStrict . T.encodeUtf8 . T.pack . show

-- Pretty printing

printAsyncRequest :: AsyncRequest -> IO ()
printAsyncRequest (UpdateRequest _ _ _ method arg) =
    printf "→ update %s%s\n" method (shorten 60 (candidOrPretty arg))

printSyncRequest :: SyncRequest -> IO ()
printSyncRequest (ReadStateRequest _ _ paths) =
    printf "→ state? %s\n" (intercalate ", " $ map (intercalate "/" . map show) paths)
printSyncRequest (QueryRequest _ _ _ method arg) =
    printf "→ query %s%s\n" method (shorten 60 (candidOrPretty arg))

printCallResponse :: CallResponse -> IO ()
printCallResponse (Rejected (c, s)) =
    printf "← rejected (%s): %s\n" (show c) s
printCallResponse (Replied blob) =
    printf "← replied: %s\n" (shorten 100 (candidOrPretty blob))

printReqStatus :: RequestStatus -> IO ()
printReqStatus Received =
    printf "← received\n"
printReqStatus Processing =
    printf "← processing\n"
printReqStatus (CallResponse c) = printCallResponse c

printReqResponse :: ReqResponse -> IO ()
printReqResponse (QueryResponse c) = printCallResponse c
printReqResponse (ReadStateResponse _ ) = error "dead code in ic-ref"

candidOrPretty :: Blob -> String
candidOrPretty b
  | BC.pack "DIDL" `B.isPrefixOf` b
  , Right vs <- Candid.decodeVals b
  = show (pretty vs)
  | otherwise
  = "(" ++ prettyBlob b ++ ")"


shorten :: Int -> String -> String
shorten n s = a ++ (if null b then "" else "…")
  where (a,b) = splitAt n s


submitAndRun :: IO RequestID -> AsyncRequest -> DRun ()
submitAndRun mkRid r = do
    lift $ printAsyncRequest r
    rid <- lift mkRid
    submitRequest rid r
    runToCompletion
    -- TODO: Add a backdoor to IC.Ref, no need to go through certification here
    {-
    (_wants_time, r) <- readRequest (StatusRequest dummyExpiry rid)
    lift $ printReqStatus r
    -}

submitRead :: SyncRequest -> DRun ()
submitRead r = do
    lift $ printSyncRequest r
    t <- lift getTimestamp
    _r <- readRequest t r
    -- TODO: Add a backdoor to IC.Ref, no need to go through certification here
    -- lift $ printReqStatus r
    return ()
  where
    getTimestamp :: IO Timestamp
    getTimestamp = do
        t <- getPOSIXTime
        return $ Timestamp $ round (t * 1000_000_000)

newRequestIdProvider :: IO (IO RequestID)
newRequestIdProvider = do
  ref <- newIORef 0
  return $ do
    modifyIORef ref (+1)
    i <- readIORef ref
    return $ B.toLazyByteString $ B.word64BE i

callManagement :: forall s a b.
  KnownSymbol s =>
  (a -> IO b) ~ (ICManagement IO .! s) =>
  Candid.CandidArg a =>
  IO RequestID -> EntityId -> Label s -> a -> StateT IC IO ()
callManagement getRid user_id l x =
  submitAndRun getRid $
    UpdateRequest dummyExpiry (EntityId mempty) user_id (symbolVal l) (Candid.encode x)


work :: FilePath -> IO ()
work msg_file = do
  msgs <- parseFile msg_file

  let user_id = dummyUserId
  getRid <- newRequestIdProvider
  ic <- initialIC
  flip evalStateT ic $
    forM_ msgs $ \case
      Create ->
        callManagement getRid user_id #create_canister ()
      Install cid filename arg -> do
        wasm <- liftIO $ B.readFile filename
        callManagement getRid user_id #install_code $ empty
          .+ #mode .== V.IsJust #install ()
          .+ #canister_id .== Candid.Principal cid
          .+ #wasm_module .== wasm
          .+ #arg .== arg
          .+ #compute_allocation .== Nothing
          .+ #memory_allocation .== Nothing
      Reinstall cid filename arg -> do
        wasm <- liftIO $ B.readFile filename
        callManagement getRid user_id #install_code $ empty
          .+ #mode .== V.IsJust #reinstall ()
          .+ #canister_id .== Candid.Principal cid
          .+ #wasm_module .== wasm
          .+ #arg .== arg
          .+ #compute_allocation .== Nothing
          .+ #memory_allocation .== Nothing
      Upgrade cid filename arg -> do
        wasm <- liftIO $ B.readFile filename
        callManagement getRid user_id #install_code $ empty
          .+ #mode .== V.IsJust #upgrade ()
          .+ #canister_id .== Candid.Principal cid
          .+ #wasm_module .== wasm
          .+ #arg .== arg
          .+ #compute_allocation .== Nothing
          .+ #memory_allocation .== Nothing
      Query  cid method arg ->
        submitRead  (QueryRequest dummyExpiry (EntityId cid) user_id method arg)
      Update cid method arg ->
        submitAndRun getRid (UpdateRequest dummyExpiry (EntityId cid) user_id method arg)

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> versions <*> parser)
  (  fullDesc
  <> header ("Internet Computer canister runner " <> T.unpack implVersion)
  <> progDesc "This runs an IC canister against a list of messages."
  )
  where
    versions :: Parser (a -> a)
    versions =
          infoOption (T.unpack implVersion) (long "version" <> help "show version number")
      <*> infoOption (T.unpack specVersion) (long "spec-version" <> help "show spec version number")
    parser :: Parser (IO ())
    parser = work
      <$  strOption
          (  long "config"
          <> short 'c'
          <> metavar "CONFIG"
          <> value ""
          )
      <*> strArgument
          (  metavar "script"
          <> help "messages to execute"
              )
