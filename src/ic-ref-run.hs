{-# LANGUAGE ConstraintKinds #-}
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
import Data.IORef
import Data.Text.Prettyprint.Doc (pretty)

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

-- Pretty printing

dummyUserId :: CanisterId
dummyUserId = EntityId $ B.pack [0xCA, 0xFF, 0xEE]

dummyRequestId :: AsyncRequest -> RequestID
dummyRequestId = B.fromStrict . T.encodeUtf8 . T.pack . show

printAsyncRequest :: AsyncRequest -> IO ()
printAsyncRequest (UpdateRequest _ _ method arg) =
    printf "→ update %s%s\n" method (shorten 60 (candidOrPretty arg))

printSyncRequest :: SyncRequest -> IO ()
printSyncRequest (StatusRequest rid) =
    printf "→ status? %s\n" (candidOrPretty rid)
printSyncRequest (QueryRequest _ _ method arg) =
    printf "→ query %s%s\n" method (shorten 60 (candidOrPretty arg))

printReqStatus :: RequestStatus -> IO ()
printReqStatus Unknown =
    printf "← unknown\n"
printReqStatus Received =
    printf "← received\n"
printReqStatus Processing =
    printf "← processing\n"
printReqStatus (Rejected (c, s)) =
    printf "← rejected (%s): %s\n" (show c) s
printReqStatus (Completed CompleteUnit) =
    printf "← completed\n"
printReqStatus (Completed (CompleteCanisterId id)) =
    printf "← completed: canister-id = %s\n" (prettyID id)
printReqStatus (Completed (CompleteArg blob)) =
    printf "← completed: %s\n" (shorten 100 (candidOrPretty blob))

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


submitAndRun :: IO RequestID -> AsyncRequest -> DRun ReqResponse
submitAndRun mkRid r = do
    lift $ printAsyncRequest r
    rid <- lift mkRid
    submitRequest rid r
    runToCompletion
    r <- readRequest (StatusRequest rid)
    lift $ printReqStatus r
    return r

submitRead :: SyncRequest -> DRun ReqResponse
submitRead r = do
    lift $ printSyncRequest r
    r <- readRequest r
    lift $ printReqStatus r
    return r

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
  void $ submitAndRun getRid $
    UpdateRequest (EntityId mempty) user_id (symbolVal l) (Candid.encode x)


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
      Reinstall cid filename arg -> do
        wasm <- liftIO $ B.readFile filename
        callManagement getRid user_id #install_code $ empty
          .+ #mode .== V.IsJust #reinstall ()
          .+ #canister_id .== Candid.Principal cid
          .+ #wasm_module .== wasm
          .+ #arg .== arg
          .+ #compute_allocation .== Nothing
      Upgrade cid filename arg -> do
        wasm <- liftIO $ B.readFile filename
        callManagement getRid user_id #install_code $ empty
          .+ #mode .== V.IsJust #upgrade ()
          .+ #canister_id .== Candid.Principal cid
          .+ #wasm_module .== wasm
          .+ #arg .== arg
          .+ #compute_allocation .== Nothing
      Query  cid method arg ->
        void $ submitRead  (QueryRequest (EntityId cid) user_id method arg)
      Update cid method arg ->
        void $ submitAndRun getRid (UpdateRequest (EntityId cid) user_id method arg)

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
