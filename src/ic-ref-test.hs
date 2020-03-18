{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

-- Todo: Break module into plumbing and actual tests

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.HashMap.Lazy as HM
import Network.HTTP.Client
import Network.HTTP.Types
import Numeric.Natural
import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Runners.Html
import Control.Monad.Trans
import Control.Concurrent
import Control.Monad
import System.FilePath
import System.IO
import Data.IORef
import Control.Exception
import System.Directory
import System.Environment
import System.Exit
import System.Process.ByteString.Lazy
import System.Random

import IC.Version
import IC.HTTP.GenR
import IC.HTTP.GenR.Parse
import IC.HTTP.CBOR (decode, encode)
import IC.HTTP.RequestId
import IC.Test.Options

type Blob = BS.ByteString

main :: IO ()
main = withTestSuitePrimer $ \primeTestSuite ->
    defaultMainWithIngredients ingredients (tests primeTestSuite)
  where
    ingredients = htmlRunner : defaultIngredients ++ [includingOptions [endpointOption]]

-- | This helper function runs the main action, and passes a way to prime the
-- test suite. If this primer is _not_ executed, the program will always
-- return success (but still list failing tests)
withTestSuitePrimer :: (IO () -> IO ()) -> IO ()
withTestSuitePrimer main = do
    specCompliant <- newIORef False
    let primeTestSuite = writeIORef specCompliant True
    main primeTestSuite `catch`
        (\(e :: ExitCode) -> readIORef specCompliant >>= \case
            False -> throwIO ExitSuccess
            True -> throwIO e)


tests :: IO () -> TestTree
tests primeTestSuite = testGroup "Tests" [icTests primeTestSuite, idTests]

-- To use IC.HTTP.CBOR with HUnit
instance Parse IO where parseError = assertFailure . T.unpack

asRight :: HasCallStack => Either T.Text a -> IO a
asRight (Left err) = assertFailure (T.unpack err)
asRight (Right gr) = return gr

getCBOR :: HasCallStack => Endpoint -> String -> IO (Response BS.ByteString)
getCBOR (Endpoint endpoint) path = do
  -- this uses a fresh manger for each request. If we care about performance,
  -- this could revisited (to share connections)
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ endpoint ++ path
  httpLbs request manager

postCBOR :: HasCallStack => Endpoint -> String -> GenR -> IO (Response BS.ByteString)
postCBOR (Endpoint endpoint) path gr = do
  -- this uses a fresh manger for each request. If we care about performance,
  -- this could revisited (to share connections)
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ endpoint ++ path
  request <- return $ request
    { method = "POST"
    , requestBody = RequestBodyLBS $ BS.toLazyByteString $ encode gr
    , requestHeaders = [(hContentType, "application/cbor")]
    }
  httpLbs request manager

pollDelay :: IO ()
pollDelay = threadDelay $ 10 * 1000 -- 10 milliseonds

loop :: HasCallStack => IO (Maybe a) -> IO a
loop act = go (0::Int)
  where
    go 10000 = assertFailure "Polling timed out"
    go n = act >>= \case
      Just r -> return r
      Nothing -> go (n+1)

submitCBOR :: Endpoint -> GenR -> IO GenR
submitCBOR ep req = do
  res <- postCBOR ep "/api/v1/submit" $ envelope req
  code2xx res -- the body is unspecified
  loop $ do
    pollDelay
    res <- postCBOR ep "/api/v1/read" $ envelope $ rec
      [ "request_type" =: GText "request_status"
      , "request_id" =: GBlob (requestId req)
      ]
    gr <- okCBOR res
    flip record gr $ do
        s <- field text "status"
        case s of
          "unknown" -> return Nothing
          "processing" -> return Nothing
          "received" -> return Nothing
          "replied" -> swallowAllFields >> return (Just gr)
          "rejected" -> swallowAllFields >> return (Just gr)
          _ -> lift $ assertFailure $ "Unexpected status " ++ show s

readCBOR :: Endpoint -> GenR -> IO GenR
readCBOR ep req = postCBOR ep "/api/v1/read" (envelope req) >>= okCBOR

okCBOR :: HasCallStack => Response BS.ByteString -> IO GenR
okCBOR response = do
  code2xx response
  asRight $ decode $ responseBody response

emptyRec :: HasCallStack => GenR -> IO ()
emptyRec = \case
  GRec hm | HM.null hm -> return ()
  _ -> assertFailure "Not an empty record"

statusUnknown :: HasCallStack => GenR -> IO ()
statusUnknown = record $ do
    s <- field text "status"
    lift $ s @?= "unknown"

statusReply :: HasCallStack => GenR -> IO GenR
statusReply = record $ do
    s <- field text "status"
    if s == "replied"
    then field anyType "reply"
    else do
      extra <-
        if s == "rejected"
        then ("\n" ++) . T.unpack <$> field text "reject_message"
        else return ""
      lift $ assertFailure $ "expected status == \"replied\" but got " ++ show s ++ extra

statusReject :: HasCallStack => Natural -> GenR -> IO ()
statusReject code = record $ do
  s <- field text "status"
  lift $ s @?= "rejected"
  n <- field nat "reject_code"
  lift $ n @?= code
  _ <- field text "reject_message"
  return ()

code4xx :: HasCallStack => Response BS.ByteString -> IO ()
code4xx response = assertBool
    ("Status " ++ show c ++ " is not 4xx")
    (400 <= c && c < 500)
  where c = statusCode (responseStatus response)

code2xx :: HasCallStack => Response BS.ByteString -> IO ()
code2xx response = assertBool
    ("Status " ++ show c ++ " is not 2xx\n" ++ msg)
    (200 <= c && c < 300)
  where
    c = statusCode (responseStatus response)
    msg = T.unpack (T.decodeUtf8 (BS.toStrict (responseBody response)))


-- Runs test once for each field with that field removed, including
-- nested fields
omitFields :: GenR -> (GenR -> Assertion) -> [TestTree]
omitFields (GRec hm) act =
    [ let hm' = HM.delete f hm
      in testCase ("omitting " ++ T.unpack f) $ act (GRec hm')
    | f <- fields
    ] ++ concat
    [ omitFields val $ \val' -> act (GRec (HM.insert f val' hm))
    | f <- fields
    , val@(GRec _) <- return $ hm HM.! f
    ]
  where fields = sort $ HM.keys hm
omitFields _ _ = error "omitFields needs a GRec"


doesn'tExist :: Blob
doesn'tExist = "\xDE\xAD\xBE\xEF"


queryToNonExistant :: GenR
queryToNonExistant = rec
    [ "request_type" =: GText "query"
    , "sender" =: GBlob doesn'tExist
    , "canister_id" =: GBlob doesn'tExist
    , "method_name" =: GText "foo"
    , "arg" =: GBlob "nothing to see here"
    ]

requestStatusNonExistant :: GenR
requestStatusNonExistant = rec
    [ "request_type" =: GText "request_status"
    , "request_id" =: GBlob doesn'tExist
    ]

-- Only to test which fields are required
dummyInstall :: GenR
dummyInstall = rec
    [ "request_type" =: GText "install_code"
    , "sender" =: GBlob doesn'tExist
    , "canister_id" =: GBlob doesn'tExist
    , "module" =: GBlob ""
    , "arg" =: GBlob ""
    ]

envelope :: GenR -> GenR
envelope content = rec
    [ "sender_pubkey" =: GBlob "dummy_pubkey"
    , "sender_sig" =: GBlob "dummy_sig"
    , "content" =: content
    ]

-- We are using randomness to get fresh ids.
-- Not the most principled way, but I don’t see much better ways.
getFreshId :: IO BS.ByteString
getFreshId = BS.pack <$> replicateM 16 randomIO

getTestDir :: IO FilePath
getTestDir =
    lookupEnv "IC_TEST_DATA" >>= \case
    Just fp -> return fp
    Nothing -> do
      -- nix use
      exePath <- getExecutablePath
      let exeRelPath = takeDirectory exePath </> "../test-data"
      -- convenient for cabal new-run use
      try [ exeRelPath, "test-data", "../test-data", "impl/test-data" ]
  where
    try (f:fs) = doesDirectoryExist f >>= \case
      True -> return f
      False -> try fs
    try [] = error "getTestDir: Please set IC_TEST_DATA"

getTestWasm :: FilePath -> IO BS.ByteString
getTestWasm wat = do
  dir <- getTestDir
  (code, out, err) <- readProcessWithExitCode "wat2wasm"
    [ dir </> wat <.> "wat", "-o", "/dev/stdout" ] ""
  BS.hPutStr stderr err
  if code /= ExitSuccess
  then error "getTestWasm failed"
  else return out


icTests :: IO () -> TestTree
icTests primeTestSuite = askOption $ \ep -> testGroup "Public Spec acceptance tests"
  [ testCase "status endpoint" $ do
      gr <- getCBOR ep "/api/v1/status" >>= okCBOR
      flip record gr $ do
        v <- field text "ic_api_version"
        _ <- optionalField text "impl_source"
        _ <- optionalField text "impl_version"
        _ <- optionalField text "impl_revision"

        lift $ unless (v == specVersion || v == "unreleased") $
            assertFailure $ "ic_api_version should be " ++ show specVersion ++ " or \"unversioned\", not " ++ show v

  , testCase "spec compliance claimed" $ do
      gr <- getCBOR ep "/api/v1/status" >>= okCBOR
      flip record gr $ do
        v <- field text "ic_api_version"
        _ <- optionalField text "impl_source"
        _ <- optionalField text "impl_version"
        _ <- optionalField text "impl_revision"

        lift $ unless (v == specVersion) $
            assertFailure $ "ic_api_version is not " ++ show specVersion ++ ", so no spec compliance assumed"

        -- The system claims to be spec-compliant.
        -- So lets make sure the error code of ic-ref-test reflects reality
        lift primeTestSuite

  , testGroup "create_canister"
    [ testCase "no id given" $ do
        gr <- submitCBOR ep $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob ""
          ]
        _can_id <- statusReply gr >>= record (field blob "canister_id")
        return ()
    , testCaseSteps "desired id" $ \step -> do
        step "Creating"
        id <- getFreshId
        gr <- submitCBOR ep $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob ""
          , "desired_id" =: GBlob id
          ]
        id' <- statusReply gr >>= record (field blob "canister_id")
        id' @?= id

        step "Creating again"
        gr <- submitCBOR ep $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob ""
          , "desired_id" =: GBlob id
          ]
        statusReject 3 gr
    ]

  , testGroup "install"
    [ testGroup "required fields" $
        omitFields (envelope dummyInstall) $ \req ->
          postCBOR ep "/api/v1/submit" req >>= code4xx

    , testCaseSteps "trivial wasm module" $ \step -> do
        step "Create"
        gr <- submitCBOR ep $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob ""
          ]
        can_id <- statusReply gr >>= record (field blob "canister_id")

        step "Install"
        wasm <- getTestWasm "trivial"
        gr <- submitCBOR ep $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob ""
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob wasm
          , "arg" =: GBlob ""
          ]
        r <- statusReply gr
        flip record r $ return ()

        step "Install"
        gr <- submitCBOR ep $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob ""
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob wasm
          , "arg" =: GBlob ""
          , "mode" =: GText "install" -- NB: This is the default
          ]
        statusReject 3 gr

        step "Reinstall"
        wasm <- getTestWasm "trivial"
        gr <- submitCBOR ep $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob ""
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob wasm
          , "arg" =: GBlob ""
          , "mode" =: GText "reinstall"
          ]
        r <- statusReply gr
        flip record r $ return ()

        step "Upgrade"
        wasm <- getTestWasm "trivial"
        gr <- submitCBOR ep $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob ""
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob wasm
          , "arg" =: GBlob ""
          , "mode" =: GText "upgrade"
          ]
        r <- statusReply gr
        flip record r $ return ()

  , testCaseSteps "universal wasm module" $ \step -> do
      step "Create"
      gr <- submitCBOR ep $ rec
        [ "request_type" =: GText "create_canister"
        , "sender" =: GBlob ""
        ]
      can_id <- statusReply gr >>= record (field blob "canister_id")

      step "Install"
      wasm <- getTestWasm "universal"
      gr <- submitCBOR ep $ rec
        [ "request_type" =: GText "install_code"
        , "sender" =: GBlob ""
        , "canister_id" =: GBlob can_id
        , "module" =: GBlob wasm
        , "arg" =: GBlob ""
        ]
      r <- statusReply gr
      flip record r $ return ()

      let
        call :: HasCallStack => T.Text -> Blob -> IO Blob
        call method_name arg = do
          gr <- submitCBOR ep $ rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob "1234"
            , "canister_id" =: GBlob can_id
            , "method_name" =: GText method_name
            , "arg" =: GBlob arg
            ]
          statusReply gr >>= record (field blob "arg")

        callReject :: HasCallStack => T.Text -> Blob -> Natural -> IO ()
        callReject method_name arg code = do
          gr <- submitCBOR ep $ rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob "1234"
            , "canister_id" =: GBlob can_id
            , "method_name" =: GText method_name
            , "arg" =: GBlob arg
            ]
          statusReject code gr

      let
        query :: HasCallStack => T.Text -> Blob -> IO Blob
        query method_name arg = do
          gr <- readCBOR ep $ rec
            [ "request_type" =: GText "query"
            , "sender" =: GBlob "1234"
            , "canister_id" =: GBlob can_id
            , "method_name" =: GText method_name
            , "arg" =: GBlob arg
            ]
          statusReply gr >>= record (field blob "arg")

        queryReject :: HasCallStack => T.Text -> Blob -> Natural -> IO ()
        queryReject method_name arg code = do
          gr <- readCBOR ep $ rec
            [ "request_type" =: GText "query"
            , "sender" =: GBlob "1234"
            , "canister_id" =: GBlob can_id
            , "method_name" =: GText method_name
            , "arg" =: GBlob arg
            ]
          statusReject code gr


      step "Call (update)"
      r <- call "get_state" "ABCD"
      r @?= ("INIT" <> "\x0\x0\x0\x0" <> "ABCD" <> "1234")

      step "Call (query)"
      r <- query "get_state_query" "ABCD"
      r @?= ("INIT" <> "\x0\x0\x0\x0" <> "ABCD" <> "1234")

      step "Non-existing call"
      callReject "does_not_exist" "" 3
      step "Non-existing query"
      queryReject "does_not_exist" "" 3

      step "canister_self"
      r <- query "get_canister_self" ""
      r @?= can_id

      step "reject (update)"
      callReject "reject" "FOOBAR" 4

      step "reject (query)"
      queryReject "reject_query" "FOOBAR" 4

      -- https://www.wordnik.com/lists/really-cool-four-letter-words
      step "Set mem (update)"
      r <- call "set_mem" "ibis"
      r @?= "INIT"
      r <- query "get_state_query" "ABCD"
      r @?= ("ibis" <> "\x0\x0\x0\x0" <> "ABCD" <> "1234")

      step "Set mem (query)"
      r <- query "set_mem_query" "pelf"
      r @?= "ibis"
      r <- query "get_state_query" "ABCD"
      r @?= ("ibis" <> "\x0\x0\x0\x0" <> "ABCD" <> "1234")

      step "Set stable mem (failing)"
      callReject "set_stable_mem" "sofa" 5
      r <- query "get_state_query" "ABCD"
      r @?= ("ibis" <> "\x0\x0\x0\x0" <> "ABCD" <> "1234")

      step "Set stable mem (failing, query)"
      queryReject "set_stable_mem_query" "sofa" 5
      r <- query "get_state_query" "ABCD"
      r @?= ("ibis" <> "\x0\x0\x0\x0" <> "ABCD" <> "1234")

      step "Grow stable memory (query)"
      r <- query "grow_stable_mem_query" ""
      r @?= "\x0\x0\x0\x0"

      step "Grow stable memory (update)"
      r <- call "grow_stable_mem" ""
      r @?= "\x0\x0\x0\x0"
      r <- call "grow_stable_mem" ""
      r @?= "\x1\x0\x0\x0"

      step "Set stable mem"
      r <- call "set_stable_mem" "iota"
      r @?= "\x0\x0\x0\x0"
      r <- query "get_state_query" "ABCD"
      r @?= ("ibis" <> "\x0\x0\x0\x0" <> "iota" <> "ABCD" <> "1234")

      step "Set stable mem (query)"
      r <- query "set_stable_mem_query" "oboe"
      r @?= "iota"
      r <- query "get_state_query" "ABCD"
      r @?= ("ibis" <> "\x0\x0\x0\x0" <> "iota" <> "ABCD" <> "1234")

      step "No reply"
      callReject "no_reply" "" 5
      step "No reply (query)"
      queryReject "no_reply_query" "" 5

      step "Self-call"
      r <- call "forward_call" can_id
      r @?= ("ibis" <> "\x0\x0\x0\x0" <> "iota" <> can_id)

      step "Self-call (query)"
      queryReject "forward_call" can_id 3

      step "Inter-canister call to non-existant"
      r <- call "forward_call" doesn'tExist
      BS.take 4 r @?= "\x03\x0\x0\x0"
    ]

  , testGroup "query"
    [ testGroup "required fields" $
        omitFields (envelope queryToNonExistant) $ \req ->
          postCBOR ep "/api/v1/read" req >>= code4xx

    , testCase "non-existing canister" $ do
        gr <- postCBOR ep "/api/v1/read" (envelope queryToNonExistant) >>= okCBOR
        statusReject 3 gr
    ]

  , testGroup "request_status"
    [ testCase "unknown request" $ do
        gr <- postCBOR ep "/api/v1/read" (envelope requestStatusNonExistant) >>= okCBOR
        statusUnknown gr
    , testGroup "required fields" $
        omitFields (envelope requestStatusNonExistant) $ \req ->
          postCBOR ep "/api/v1/read" req >>= code4xx
    ]
  ]


idTests :: TestTree
idTests = testGroup "Reference request id calculation"
  [ testCase "example from public spec" $
     let gr = GRec $ mconcat
          [ "request_type" =: GText "call"
          , "canister_id" =: GBlob "\x00\x00\x00\x00\x00\x00\x04\xD2"
          , "method_name" =: GText "hello"
          , "arg" =: GBlob "DIDL\x00\xFD*"
          ]
      in requestId gr @?= "\x87\x81\x29\x1c\x34\x7d\xb3\x2a\x9d\x8c\x10\xeb\x62\xb7\x10\xfc\xe5\xa9\x3b\xe6\x76\x47\x4c\x42\xba\xbc\x74\xc5\x18\x58\xf9\x4b"
  ]
