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
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Builder as B
import qualified Data.Text as T
import Control.Monad.Trans
import Control.Monad.Trans.State
import Text.Printf
import Data.List
import Prettyprinter (pretty)
import Data.Time.Clock.POSIX
import Control.Monad.Random.Lazy

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Row (empty, (.==), (.+), type (.!), Label)
import qualified Codec.Candid as Candid
import qualified Data.Row.Variants as V
import qualified Data.Word as W


import IC.HTTP.Request
import IC.Version
import IC.Types
import IC.Ref
import IC.DRun.Parse (Ingress(..), parseFile)
import IC.Management

import IC.StateFile
import IC.Serialise ()

type DRun = StateT IC IO

-- Pretty printing

printCallRequest :: CallRequest -> IO ()
printCallRequest (CallRequest _ _ method arg) =
    printf "=> update %s%s\n" method (shorten 60 (candidOrPretty arg))

printReadStateRequest :: ReadStateRequest -> IO ()
printReadStateRequest (ReadStateRequest _ paths) =
    printf "=> state? %s\n" (intercalate ", " $ map (intercalate "/" . map show) paths)

printQueryRequest :: QueryRequest -> IO ()
printQueryRequest (QueryRequest _ _ method arg) =
    printf "=> query %s%s\n" method (shorten 60 (candidOrPretty arg))

printCallResponse :: CallResponse -> IO ()
printCallResponse (Rejected (c, s, _err)) =
    printf "<= rejected (%s): %s\n" (show c) s
printCallResponse (Replied blob) =
    printf "<= replied: %s\n" (shorten 100 (candidOrPretty blob))

printReqStatus :: RequestStatus -> IO ()
printReqStatus Received =
    printf "<= received\n"
printReqStatus Processing =
    printf "<= processing\n"
printReqStatus (CallResponse c) = printCallResponse c

printReqResponse :: ReqResponse -> IO ()
printReqResponse (QueryResponse c) = printCallResponse c
printReqResponse (ReadStateResponse _ ) = error "dead code in ic-ref"

candidOrPretty :: Blob -> String
candidOrPretty b
  | BC.pack "DIDL" `B.isPrefixOf` b
  , Right (_, vs) <- Candid.decodeVals b
  = show (pretty vs)
  | otherwise
  = "(" ++ prettyBlob b ++ ")"


shorten :: Int -> String -> String
shorten n s = a ++ (if null b then "" else "...")
  where (a,b) = splitAt n s


submitAndRun :: CanisterId -> CallRequest -> DRun ()
submitAndRun ecid r = do
    lift $ printCallRequest r
    rid <- lift mkRequestId
    submitRequest rid r ecid
    runToCompletion
    (r, _) <- gets (snd . (M.! rid) . requests)
    lift $ printReqStatus r

submitQuery :: QueryRequest -> DRun ()
submitQuery r = do
    lift $ printQueryRequest r
    t <- lift getTimestamp
    r <- handleQuery t r
    lift $ printReqResponse r
  where
    getTimestamp :: IO Timestamp
    getTimestamp = do
        t <- getPOSIXTime
        return $ Timestamp $ round (t * 1000_000_000)

mkRequestId :: IO RequestID
mkRequestId = B.toLazyByteString . B.word64BE <$> randomIO

callManagement :: forall s a b.
  KnownSymbol s =>
  (a -> IO b) ~ (ICManagement IO .! s) =>
  Candid.CandidArg a =>
  CanisterId -> EntityId -> Label s -> a -> StateT IC IO ()
callManagement ecid user_id l x =
  submitAndRun ecid $
    CallRequest (EntityId mempty) user_id (symbolVal l) (Candid.encode x)

work :: [(SubnetType, String, [(W.Word64, W.Word64)])] -> Int -> FilePath -> IO ()
work subnets systemTaskPeriod msg_file = do
  let subs = map (\(t, n, ranges) -> SubnetConfig t n ranges) subnets
  msgs <- parseFile msg_file

  let user_id = dummyUserId
  withStore (initialIC subs) Nothing $ \store ->
      withAsync (loopIC store) $ \_async ->
        --flip evalStateT ic $
          forM_ msgs $ \msg -> modifyStore store $
           case msg of
            Create ecid ->
              callManagement (EntityId ecid) user_id #provisional_create_canister_with_cycles $ empty
                .+ #settings .== Nothing
                .+ #amount .== Nothing
            Install cid filename arg -> do
              wasm <- liftIO $ B.readFile filename
              callManagement (EntityId cid) user_id #install_code $ empty
                .+ #mode .== V.IsJust #install ()
                .+ #canister_id .== Candid.Principal cid
                .+ #wasm_module .== wasm
                .+ #arg .== arg
            Reinstall cid filename arg -> do
              wasm <- liftIO $ B.readFile filename
              callManagement (EntityId cid) user_id #install_code $ empty
                .+ #mode .== V.IsJust #reinstall ()
                .+ #canister_id .== Candid.Principal cid
                .+ #wasm_module .== wasm
                .+ #arg .== arg
            Upgrade cid filename arg -> do
              wasm <- liftIO $ B.readFile filename
              callManagement (EntityId cid) user_id #install_code $ empty
                .+ #mode .== V.IsJust #upgrade ()
                .+ #canister_id .== Candid.Principal cid
                .+ #wasm_module .== wasm
                .+ #arg .== arg
            Query  cid method arg ->
              submitQuery (QueryRequest (EntityId cid) user_id method arg)
            Update cid method arg ->
              submitAndRun (EntityId cid) (CallRequest (EntityId cid) user_id method arg)
  where
    loopIC :: Store IC -> IO ()
    loopIC store = forever $ do
        threadDelay systemTaskPeriod
        modifyStore store aux
      where
        aux = do
          lift getTimestamp >>= setAllTimesTo
          processSystemTasks

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
    canister_ids_per_subnet :: W.Word64
    canister_ids_per_subnet = 1_048_576
    range :: W.Word64 -> (W.Word64, W.Word64)
    range n = (n * canister_ids_per_subnet, (n + 1) * canister_ids_per_subnet - 1)
    defaultSubnetConfig :: [(SubnetType, String, [(W.Word64, W.Word64)])]
    defaultSubnetConfig = [(System, "sk1", [range 0]), (Application, "sk2", [range 1])]
    defaultSystemTaskPeriod :: Int
    defaultSystemTaskPeriod = 1
    parser :: Parser (IO ())
    parser = work
      <$  strOption
          (  long "config"
          <> short 'c'
          <> metavar "CONFIG"
          <> value ""
          )
      <*> (
            (
              option auto
              (  long "subnet-config"
              <> help ("choose initial subnet configurations (default: " ++ show defaultSubnetConfig ++ ")")
              )
            )
          <|> pure defaultSubnetConfig
          )
      <*>
        (
          (
            option auto
            (  long "system-task-period"
            <> help ("choose execution period (in integer seconds) for system tasks, i.e., heartbeats and global timers (default: " ++ show defaultSystemTaskPeriod ++ ")")
            )
          )
        <|> pure defaultSystemTaskPeriod
        )
      <*> strArgument
          (  metavar "script"
          <> help "messages to execute"
              )
