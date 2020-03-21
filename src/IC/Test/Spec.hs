{- |

This module contains a test suite for the Internet Computer

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IC.Test.Spec (icTests) where

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
import Control.Monad.Trans
import Control.Concurrent
import Control.Monad
import Data.Functor
import System.FilePath
import System.IO
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
import IC.Crypto
import IC.Id.Forms hiding (Blob)
import IC.Test.Options
import IC.Test.Universal

-- * Test data, standard requests

doesn'tExist :: Blob
doesn'tExist = "\xDE\xAD\xBE\xEF" -- hopefully no such canister/user exists


defaultSK :: SecretKey
defaultSK = createSecretKey "fixed32byteseedfortesting"

otherSK :: SecretKey
otherSK = createSecretKey "anotherfixed32byteseedfortesting"

defaultUser :: Blob
defaultUser = mkSelfAuthenticatingId $ toPublicKey defaultSK
otherUser :: Blob
otherUser = mkSelfAuthenticatingId $ toPublicKey otherSK

queryToNonExistant :: GenR
queryToNonExistant = rec
    [ "request_type" =: GText "query"
    , "sender" =: GBlob defaultUser
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
minimalInstall :: GenR
minimalInstall = rec
    [ "request_type" =: GText "install_code"
    , "sender" =: GBlob defaultUser
    , "canister_id" =: GBlob doesn'tExist
    , "module" =: GBlob ""
    , "arg" =: GBlob ""
    ]

envelope :: SecretKey -> GenR -> GenR
envelope sk content = rec
    [ "sender_pubkey" =: GBlob (toPublicKey sk)
    , "sender_sig" =: GBlob (sign sk (requestId content))
    , "content" =: content
    ]

badEnvelope :: GenR -> GenR
badEnvelope content = rec
    [ "sender_pubkey" =: GBlob (toPublicKey defaultSK)
    , "sender_sig" =: GBlob (BS.replicate 64 0x42)
    , "content" =: content
    ]

-- * The actual test suite (see below for helper functions)

icTests :: IO () -> TestTree
icTests primeTestSuite = askOption $ \ep -> testGroup "Public Spec acceptance tests"
  [ testCase "status endpoint" $ do
      api_version <- getLBS ep "/api/v1/status" >>= okCBOR >>= statusResonse
      unless (api_version == specVersion || api_version == "unversioned") $
        assertFailure $ "ic_api_version should be " ++ show specVersion ++
                        " or \"unversioned\", not " ++ show api_version

  , testCase "spec compliance claimed" $ do
      api_version <- getLBS ep "/api/v1/status" >>= okCBOR >>= statusResonse

      unless (api_version == specVersion) $
        assertFailure $ "ic_api_version is not " ++ show specVersion ++ ", so no spec compliance assumed"

      -- The system claims to be spec-compliant.
      -- So lets make sure the error code of ic-ref-test reflects reality
      primeTestSuite

  , testGroup "create_canister"
    [ testCase "no id given" $ do
        cid <- submitCBOR ep >=> createReply $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          ]
        assertBool "opaque id expected" $ isOpaqueId cid
    , testCaseSteps "desired id" $ \step -> do
        step "Creating"
        bytes <- getRand8Bytes
        let id = mkDerivedId defaultUser bytes
        id' <- submitCBOR ep >=> createReply $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          , "desired_id" =: GBlob id
          ]
        id' @?= id

        step "Creating again"
        submitCBOR ep >=> statusReject 3 $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          , "desired_id" =: GBlob id
          ]

    , testCaseSteps "desired id (wrong class)" $ \step -> do
        step "Creating"
        id <- mkOpaqueId <$> getRand8Bytes
        submitCBOR ep >=> statusReject 3 $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          , "desired_id" =: GBlob id
          ]

    , testCaseSteps "desired id (wrong user)" $ \step -> do
        step "Creating"
        bytes <- getRand8Bytes
        let id = mkDerivedId otherUser bytes
        submitCBOR ep >=> statusReject 3 $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          , "desired_id" =: GBlob id
          ]
    ]

  , testGroup "install"
    [ testGroup "required fields" $
        omitFields (envelope defaultSK minimalInstall) $ \req ->
          postCBOR ep "/api/v1/submit" req >>= code4xx

    , testCaseSteps "trivial wasm module" $ \step -> do
        step "Create"
        can_id <- submitCBOR ep >=> createReply $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          ]

        step "Install"
        trivial_wasm <- getTestWat "trivial"
        submitCBOR ep >=> emptyReply $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob trivial_wasm
          , "arg" =: GBlob ""
          ]

        step "Install"
        submitCBOR ep >=> statusReject 3 $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob trivial_wasm
          , "arg" =: GBlob ""
          , "mode" =: GText "install" -- NB: This is the default
          ]

        step "Reinstall"
        submitCBOR ep >=> statusReply >=> emptyRec $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob trivial_wasm
          , "arg" =: GBlob ""
          , "mode" =: GText "reinstall"
          ]

        step "Upgrade"
        submitCBOR ep >=> emptyReply $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob trivial_wasm
          , "arg" =: GBlob ""
          , "mode" =: GText "upgrade"
          ]
    ]

  , testGroup "universal canister" $
    let
      -- Some common operations on the universal canister
      -- The primed variant (call') return the response record,
      -- e.g. to check for error conditions.
      -- The unprimed variant expect a reply.

      install' :: HasCallStack => Prog -> IO (Blob, GenR)
      install' prog = do
        cid <- submitCBOR ep >=> createReply $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          ]
        trivial_wasm <- getTestWasm "universal_canister"
        gr <- submitCBOR ep $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "module" =: GBlob trivial_wasm
          , "arg" =: GBlob (run prog)
          ]
        return (cid, gr)


      install :: HasCallStack => Prog -> IO Blob
      install prog = do
        (cid, gr) <- install' prog
        emptyReply gr
        return cid

      call' :: HasCallStack => Blob -> Prog -> IO GenR
      call' cid prog = submitCBOR ep $ rec
          [ "request_type" =: GText "call"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "update"
          , "arg" =: GBlob (run prog)
          ]

      call :: HasCallStack => Blob -> Prog -> IO Blob
      call cid prog = call' cid prog >>= callReply

      query' :: HasCallStack => Blob -> Prog -> IO GenR
      query' cid prog =
        readCBOR ep $ rec
          [ "request_type" =: GText "query"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "query"
          , "arg" =: GBlob (run prog)
          ]

      query :: HasCallStack => Blob -> Prog -> IO Blob
      query cid prog = query' cid prog >>= callReply

    in
      [ testCase "create and install" $ do
        cid <- install noop
        assertBool "opaque id expected" $ isOpaqueId cid

      , testCaseSteps "simple calls" $ \step -> do
        cid <- install noop

        step "Call"
        r <- call cid $ replyData "ABCD"
        r @?= "ABCD"

        step "Call (query)"
        r <- query cid $ replyData "ABCD"
        r @?= "ABCD"

        step "reject"
        call' cid >=> statusReject 4 $ reject "ABCD"

        step "reject (query)"
        query' cid >=> statusReject 4 $ reject "ABCD"

        step "No reply"
        call' cid >=> statusReject 5 $ noop
        step "No reply (query)"
        query' cid >=> statusReject 5 $ noop

        step "Double reply"
        call' cid >=> statusReject 5 $ reply >>> reply
        step "Double reply (query)"
        query' cid >=> statusReject 5 $ reply >>> reply

        step "Reply data append after reply"
        call' cid >=> statusReject 5 $ reply >>> replyDataAppend "foo"

        step "Reply data append after reject"
        call' cid >=> statusReject 5 $ reject "bar" >>> replyDataAppend "foo"

        step "Caller"
        r <- call cid $ replyData caller
        r @?= defaultUser

        step "Caller (query)"
        r <- query cid $ replyData caller
        r @?= defaultUser

      , testCase "self" $ do
        cid <- install noop
        r <- query cid $ replyData self
        r @?= cid

      , testCaseSteps "inter-canister calls" $ \step -> do
        cid <- install noop

        step "Call to nonexistant"
        r <- call cid $
          call_simple
            "foo"
            "bar"
            ""
            (callback replyRejectData)
            ""
        BS.take 4 r @?= "\x03\x0\x0\x0"

        let otherSide = callback $
              replyDataAppend "Hello " >>>
              replyDataAppend caller  >>>
              replyDataAppend " this is " >>>
              replyDataAppend self  >>>
              reply

        step "Call from query traps"
        query' cid >=> statusReject 5 $
          call_simple
            (bytes cid)
            "update"
            (callback replyArgData)
            (callback replyRejectData)
            otherSide

        step "Self-call (to update)"
        r <- call cid $
          call_simple
            (bytes cid)
            "update"
            (callback replyArgData)
            (callback replyRejectData)
            otherSide
        r @?= ("Hello " <> cid <> " this is " <> cid)

        step "Self-call (to query)"
        r <- call cid $
          call_simple
            (bytes cid)
            "query"
            (callback replyArgData)
            (callback replyRejectData)
            otherSide
        r @?= ("Hello " <> cid <> " this is " <> cid)

        step "Second reply in callback"
        r <- call cid $
          setGlobal "FOO" >>>
          replyData "First reply" >>>
          call_simple
            (bytes cid)
            "query"
            (callback (
              setGlobal "BAR" >>>
              replyData "Second reply"
            ))
            (callback (setGlobal "BAZ" >>> replyRejectData))
            otherSide
        r @?= "First reply"

        -- now check that the callback trapped and did not actual change the global
        -- This check is maybe not as good as we want: There is no guarantee
        -- that the IC actually tried to process the reply message before we do
        -- this query.
        r <- query cid $ replyData getGlobal
        r @?= "FOO"

        cid2 <- install noop
        step "Call to other canister (to update)"
        r <- call cid $
          call_simple
            (bytes cid2)
            "update"
            (callback replyArgData)
            (callback replyRejectData)
            otherSide
        r @?= ("Hello " <> cid <> " this is " <> cid2)

        step "Call to other canister (to query)"
        r <- call cid $
          call_simple
            (bytes cid2)
            "query"
            (callback replyArgData)
            (callback replyRejectData)
            otherSide
        r @?= ("Hello " <> cid <> " this is " <> cid2)

      , testCaseSteps "stable memory" $ \step -> do
        cid <- install noop

        step "Stable mem size is zero"
        r <- query cid $ replyData (i2b stableSize)
        r @?= "\x0\x0\x0\x0"

        step "Writing stable memory (failing)"
        call' cid >=> statusReject 5 $ stableWrite (int 0) "FOO"
        step "Set stable mem (failing, query)"
        query' cid >=> statusReject 5 $ stableWrite (int 0) "FOO"

        step "Growing stable memory"
        r <- call cid $ replyData (i2b (stableGrow (int 1)))
        r @?= "\x0\x0\x0\x0"

        step "Growing stable memory again"
        r <- call cid $ replyData (i2b (stableGrow (int 1)))
        r @?= "\x1\x0\x0\x0"

        step "Growing stable memory in query"
        r <- query cid $ replyData (i2b (stableGrow (int 1)))
        r @?= "\x2\x0\x0\x0"

        step "Stable mem size is two"
        r <- query cid $ replyData (i2b stableSize)
        r @?= "\x2\x0\x0\x0"

        step "Writing stable memory"
        r <- call cid $ stableWrite (int 0) "FOO" >>> reply
        r @?= ""

        step "Writing stable memory (query)"
        r <- query cid $ stableWrite (int 0) "BAR" >>> reply
        r @?= ""

        step "Reading stable memory"
        r <- call cid $ replyData (stableRead (int 0) (int 3))
        r @?= "FOO"

      , testCaseSteps "debug facilities" $ \step -> do
        cid <- install noop

        step "Using debug_print"
        r <- call cid $ debugPrint "ic-ref-test print" >>> reply
        r @?= ""

        step "Using debug_print (query)"
        r <- query cid $ debugPrint "ic-ref-test print" >>> reply
        r @?= ""

        step "Explicit trap"
        call' cid >=> statusReject 5 $ trap "trapping"
        step "Explicit trap (query)"
        query' cid >=> statusReject 5 $ trap "trapping"

      , testCase "caller (in init)" $ do
        cid <- install $ setGlobal caller
        r <- query cid $ replyData getGlobal
        r @?= defaultUser

      , testCase "self (in init)" $ do
        cid <- install $ setGlobal self
        r <- query cid $ replyData getGlobal
        r @?= cid

      , testGroup "trapping in init" $
        let
          failInInit pgm = do
            (cid, gr) <- install' pgm
            statusReject 5 gr

            -- canister does not exist
            query' cid >=> statusReject 3 $ noop
        in
        [ testCase "explicit trap" $ failInInit $ trap "trapping in install"
        , testCase "call" $ failInInit $ call_simple
            "dummy"
            "query"
            (callback replyArgData)
            (callback replyRejectData)
            (callback (
              replyDataAppend "Hello " >>>
              replyDataAppend caller >>>
              reply
            ))
        , testCase "reply" $ failInInit reply
        , testCase "reject" $ failInInit $ reject "rejecting in init"
        ]
      ]

    , testGroup "query"
      [ testGroup "required fields" $
          omitFields (envelope defaultSK queryToNonExistant) $ \req ->
            postCBOR ep "/api/v1/read" req >>= code4xx

      , testCase "non-existing canister" $
          postCBOR ep "/api/v1/read" (envelope defaultSK queryToNonExistant)
            >>= okCBOR >>= statusReject 3
      ]

  , testGroup "request_status"
    [ testGroup "required fields" $
        omitFields (envelope defaultSK requestStatusNonExistant) $ \req ->
          postCBOR ep "/api/v1/read" req >>= code4xx
    ]

  , testGroup "signature checking" $
    [ ("with bad signature", badEnvelope)
    , ("with wrong key", envelope otherSK)
    ] <&> \(name, env) -> testGroup name
      [ testCase "in query" $ do
        cid <- submitCBOR ep >=> createReply $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          ]
        let query = rec
              [ "request_type" =: GText "query"
              , "sender" =: GBlob defaultUser
              , "canister_id" =: GBlob cid
              , "method_name" =: GText "foo"
              , "arg" =: GBlob "nothing to see here"
              ]
        postCBOR ep "/api/v1/read" (env query) >>= code4xx
      , testCase "in unknown request status" $
          postCBOR ep "/api/v1/read/" (env requestStatusNonExistant) >>= code4xx
      , testCase "in request status" $ do
          let req = rec
                [ "request_type" =: GText "create_canister"
                , "sender" =: GBlob defaultUser
                ]
          _cid <- submitCBOR ep req >>= createReply
          let status_req = rec
                [ "request_type" =: GText "request_status"
                , "request_id" =: GBlob (requestId req)
                ]
          postCBOR ep "/api/v1/read/" (env status_req) >>= code4xx
      , testCase "in install" $ do
        cid <- submitCBOR ep >=> createReply $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          ]
        let req = rec
              [ "request_type" =: GText "install_code"
              , "sender" =: GBlob defaultUser
              , "canister_id" =: GBlob cid
              , "module" =: GBlob ""
              , "arg" =: GBlob ""
              ]
        postCBOR ep "/api/v1/submit" (env req) >>= code202_or_4xx

        -- Also check that the request was not created
        ingressDelay
        let status_req = rec
              [ "request_type" =: GText "request_status"
              , "request_id" =: GBlob (requestId req)
              ]
        postCBOR ep "/api/v1/read/" (envelope defaultSK status_req) >>= code4xx

      , testCase "in call" $ do
        cid <- submitCBOR ep >=> createReply $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          ]
        let req = rec
              [ "request_type" =: GText "call"
              , "sender" =: GBlob defaultUser
              , "canister_id" =: GBlob cid
              , "method_name" =: GText "foo"
              , "arg" =: GBlob "nothing to see here"
              ]
        postCBOR ep "/api/v1/submit" (env req) >>= code202_or_4xx

        -- Also check that the request was not created
        ingressDelay
        let status_req = rec
              [ "request_type" =: GText "request_status"
              , "request_id" =: GBlob (requestId req)
              ]
        postCBOR ep "/api/v1/read/" (envelope defaultSK status_req) >>= code4xx
      ]
  ]

type Blob = BS.ByteString

-- * HUnit error reporting integration

-- To use IC.HTTP.CBOR with HUnit
instance Parse IO where parseError = assertFailure . T.unpack

asRight :: HasCallStack => Either T.Text a -> IO a
asRight (Left err) = assertFailure (T.unpack err)
asRight (Right gr) = return gr

-- * Requests

-- | Simple GET request, returning a raw bytestring
--
-- This and similar functions use a fresh manger for each request. If we care
-- about performance, this could revisited, the benefit would be connection
-- re-use.

getLBS :: HasCallStack => Endpoint -> String -> IO (Response BS.ByteString)
getLBS (Endpoint endpoint) path = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ endpoint ++ path
  httpLbs request manager

-- | Posting a CBOR request, returning a raw bytestring
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

-- | Add envelope to CBOR request, post to "read", return decoded CBOR
readCBOR :: Endpoint -> GenR -> IO GenR
readCBOR ep req = postCBOR ep "/api/v1/read" (envelope defaultSK req) >>= okCBOR

-- | Add envelope to CVBOR, post to "submit", poll for the request response, and
-- return decoded CBOR
submitCBOR :: Endpoint -> GenR -> IO GenR
submitCBOR ep req = do
  res <- postCBOR ep "/api/v1/submit" (envelope defaultSK req)
  code202 res
  assertBool "Response body not empty" (BS.null (responseBody res))
  awaitStatus ep (requestId req)

awaitStatus :: Endpoint -> Blob -> IO GenR
awaitStatus ep rid = loop $ do
    pollDelay
    res <- postCBOR ep "/api/v1/read" $ envelope defaultSK $ rec
      [ "request_type" =: GText "request_status"
      , "request_id" =: GBlob rid
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
  where
    loop :: HasCallStack => IO (Maybe a) -> IO a
    loop act = go (0::Int)
      where
        go 10000 = assertFailure "Polling timed out"
        go n = act >>= \case
          Just r -> return r
          Nothing -> go (n+1)

pollDelay :: IO ()
pollDelay = threadDelay $ 10 * 1000 -- 10 milliseonds

-- How long to wait before checking if a request that should _not_ show up on
-- the system indeed did not show up
ingressDelay :: IO ()
ingressDelay = threadDelay $ 100 * 1000 -- 100 milliseonds


-- * HTTP Response predicates

codePred :: HasCallStack => String -> (Int -> Bool) -> Response Blob -> IO ()
codePred expt pred response = assertBool
    ("Status " ++ show c ++ " is not " ++ expt ++ "\n" ++ msg)
    (pred c)
  where
    c = statusCode (responseStatus response)
    msg = T.unpack (T.decodeUtf8 (BS.toStrict (responseBody response)))

code2xx, code202, code4xx, code202_or_4xx  :: HasCallStack => Response BS.ByteString -> IO ()
code2xx = codePred "2xx" $ \c -> 200 <= c && c < 300
code202 = codePred "202" $ \c -> c == 202
code4xx = codePred "4xx" $ \c -> 400 <= c && c < 500
code202_or_4xx = codePred "202 or 4xx" $ \c -> c == 202 || 400 <= c && c < 500

-- * CBOR decoding

okCBOR :: HasCallStack => Response BS.ByteString -> IO GenR
okCBOR response = do
  code2xx response
  asRight $ decode $ responseBody response

-- * Response predicates and parsers

emptyRec :: HasCallStack => GenR -> IO ()
emptyRec = \case
  GRec hm | HM.null hm -> return ()
  _ -> assertFailure "Not an empty record"

_statusUnknown :: HasCallStack => GenR -> IO ()
_statusUnknown = record $ do
    s <- field text "status"
    lift $ s @?= "unknown"

statusReject :: HasCallStack => Natural -> GenR -> IO ()
statusReject code = record $ do
  s <- field text "status"
  lift $ s @?= "rejected"
  n <- field nat "reject_code"
  lift $ n @?= code
  _ <- field text "reject_message"
  return ()

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

emptyReply :: HasCallStack => GenR -> IO ()
emptyReply = statusReply >=> emptyRec

createReply :: HasCallStack => GenR -> IO Blob
createReply = statusReply >=> record (field blob "canister_id")

callReply :: HasCallStack => GenR -> IO Blob
callReply = statusReply >=> record (field blob "arg")

statusResonse :: HasCallStack => GenR -> IO T.Text
statusResonse = record $ do
    v <- field text "ic_api_version"
    _ <- optionalField text "impl_source"
    _ <- optionalField text "impl_version"
    _ <- optionalField text "impl_revision"
    return v

-- * Programmatic test generation

-- | Runs test once for each field with that field removed, including nested
-- fields
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


getRand8Bytes :: IO BS.ByteString
getRand8Bytes = BS.pack <$> replicateM 8 randomIO

-- * Test data access

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

getTestWat :: FilePath -> IO BS.ByteString
getTestWat wat = do
  dir <- getTestDir
  (code, out, err) <- readProcessWithExitCode "wat2wasm"
    [ dir </> wat <.> "wat", "-o", "/dev/stdout" ] ""
  BS.hPutStr stderr err
  if code /= ExitSuccess
  then error "getTestWat failed"
  else return out

getTestWasm :: FilePath -> IO BS.ByteString
getTestWasm base = do
  dir <- getTestDir
  BS.readFile (dir </> base <.> "wasm")
