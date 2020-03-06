{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

-- Todo: Break module into plumbing and actual tests

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.HashMap.Lazy as HM
import Network.HTTP.Client
import Network.HTTP.Types
import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Trans
import Control.Concurrent
import Control.Monad
import System.FilePath
import System.IO
import System.Directory
import System.Environment
import System.Exit
import System.Process.ByteString.Lazy
import System.Random

import IC.HTTP.GenR
import IC.HTTP.GenR.Parse
import IC.HTTP.CBOR (decode, encode)
import IC.HTTP.RequestId
import IC.Test.Options

type Blob = BS.ByteString

main :: IO ()
main = defaultMainWithIngredients ingredients tests
  where
    ingredients = defaultIngredients ++ [includingOptions [endpointOption]]

tests :: TestTree
tests = testGroup "Tests" [icTests, idTests]

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

awaitCBOR :: Endpoint -> GenR -> IO GenR
awaitCBOR ep req = do
  res <- postCBOR ep "/api/v1/submit" req
  code2xx res -- the body is unspecified
  loop $ do
    pollDelay
    res <- postCBOR ep "/api/v1/read" $ rec
      [ "request_type" =: GText "request_status"
      , "request_id" =: GBlob (BS.toStrict (requestId req))
      ]
    gr <- okCBOR res
    flip record gr $ do
        s <- field text "status"
        case s of
          "unknown" -> return Nothing
          "pending" -> return Nothing -- FIXME: remove this
          "processing" -> return Nothing
          "received" -> return Nothing
          "replied" -> swallowAllFields >> return (Just gr)
          "rejected" -> swallowAllFields >> return (Just gr)
          _ -> lift $ assertFailure $ "Unexpected status " ++ show s

okCBOR :: HasCallStack => Response BS.ByteString -> IO GenR
okCBOR response = do
  code2xx response
  asRight $ decode $ responseBody response

emptyRec :: HasCallStack => GenR -> IO ()
emptyRec = \case
  GRec hm | HM.null hm -> return ()
  _ -> assertFailure "Not an empty record"

statusReply :: HasCallStack => GenR -> IO GenR
statusReply = record $ do
    s <- field text "status"
    lift $ s @?= "replied"
    field anyType "reply"

statusReject :: HasCallStack => GenR -> IO ()
statusReject = record $ do
  s <- field text "status"
  lift $ s @?= "rejected"
  n <- field nat "reject_code"
  lift $ n @?= 3
  _ <- field text "reject_message"
  return ()

code4xx :: HasCallStack => Response BS.ByteString -> IO ()
code4xx response = assertBool
    ("Status " ++ show c ++ " is not 4xx")
    (400 <= c && c < 500)
  where c = statusCode (responseStatus response)

code2xx :: HasCallStack => Response BS.ByteString -> IO ()
code2xx response = assertBool
    ("Status " ++ show c ++ " is not 2xx")
    (200 <= c && c < 300)
  where c = statusCode (responseStatus response)

-- Runs test once for each field with that field removed.
omitFields :: GenR -> (GenR -> Assertion) -> [TestTree]
omitFields (GRec hm) act =
    [ let hm' = HM.delete f hm
      in testCase ("omitting " ++ T.unpack f) $ act (GRec hm')
    | f <- fields
    ]
  where fields = sort $ HM.keys hm
omitFields _ _ = error "omitFields needs a GRec"


doesn'tExist :: Blob
doesn'tExist = "\xDE\xAD\xBE\xEF"


queryToNonExistant :: GenR
queryToNonExistant = rec
    [ "request_type" =: GText "query"
    , "canister_id" =: GBlob (BS.toStrict doesn'tExist)
    , "method_name" =: GText "foo"
    , "arg" =: GBlob "nothing to see here"
    ]

requestStatusNonExistant :: GenR
requestStatusNonExistant = rec
    [ "request_type" =: GText "request_status"
    , "request_id" =: GBlob (BS.toStrict doesn'tExist)
    ]

-- Only to test which fields are required
dummyInstall :: GenR
dummyInstall = rec
    [ "request_type" =: GText "install_code"
    -- FIXME, should not be optional: , "sender" =: GBlob (BS.toStrict doesn'tExist)
    , "canister_id" =: GBlob (BS.toStrict doesn'tExist)
    , "module" =: GBlob ""
    , "arg" =: GBlob ""
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


icTests :: TestTree
icTests = askOption $ \ep -> testGroup "Public Spec acceptance tests"
  [ testCase "status endpoint" $ do
      gr <- getCBOR ep "/api/v1/status" >>= okCBOR
      flip record gr $ do
        _ <- field text "ic_api_version"
        _ <- optionalField text "impl_source"
        _ <- optionalField text "impl_version"
        _ <- optionalField text "impl_revision"
        return ()

  , testGroup "create_canister"
    [ testCase "no id given" $ do
        gr <- awaitCBOR ep $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob ""
          ]
        r <- statusReply gr
        flip record r $ do
          _ <- field blob "canister_id"
          return ()
    , testCaseSteps "desired id" $ \step -> do
        step "Creating"
        id <- getFreshId
        gr <- awaitCBOR ep $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob ""
          , "desired_id" =: GBlob (BS.toStrict id)
          ]
        r <- statusReply gr
        flip record r $ do
          id' <- field blob "canister_id"
          lift $ id' @=? id

        step "Creating again"
        gr <- awaitCBOR ep $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob ""
          , "desired_id" =: GBlob (BS.toStrict id)
          ]
        statusReject gr
    ]

  , testGroup "install"
    [ testGroup "required fields" $
        omitFields dummyInstall $ \req ->
          postCBOR ep "/api/v1/submit" req >>= code4xx

    , testCase "trivial wasm" $ do
        wasm <- getTestWasm "trivial"
        -- FIXME: This should only work after canister has been registered
        gr <- awaitCBOR ep $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob ""
          , "canister_id" =: GBlob (BS.toStrict doesn'tExist)
          , "module" =: GBlob (BS.toStrict wasm)
          , "arg" =: GBlob ""
          -- FIXME: , "mode" =: GText "install"
          ]
        r <- statusReply gr
        flip record r $
          return ()
    ]

  , testGroup "query"
    [ testGroup "required fields" $
        omitFields queryToNonExistant $ \req ->
          postCBOR ep "/api/v1/read" req >>= code4xx

    , testCase "non-existing canister" $ do
        gr <- postCBOR ep "/api/v1/read" queryToNonExistant >>= okCBOR
        statusReject gr
    ]

  , testGroup "request_status"
    [ testGroup "required fields" $
        omitFields requestStatusNonExistant $ \req ->
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
