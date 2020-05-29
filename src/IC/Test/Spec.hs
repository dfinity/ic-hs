{- |

This module contains a test suite for the Internet Computer

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IC.Test.Spec (icTests) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
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
import System.Directory
import System.Environment
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


trivialWasmModule :: Blob
trivialWasmModule = "\0asm\1\0\0\0"

-- Only to test which fields are required
minimalInstall :: GenR
minimalInstall = rec
    [ "request_type" =: GText "install_code"
    , "sender" =: GBlob defaultUser
    , "canister_id" =: GBlob doesn'tExist
    , "module" =: GBlob trivialWasmModule
    , "arg" =: GBlob ""
    ]

addNonce :: GenR -> IO GenR
addNonce (GRec hm) | "nonce" `HM.member` hm = return (GRec hm)
addNonce (GRec hm) = do
  nonce <- getRand8Bytes
  return $ GRec $ HM.insert "nonce" (GBlob nonce) hm
addNonce r = return r

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
icTests primeTestSuite = withEndPoint $ testGroup "Public Spec acceptance tests"
  [ testCase "status endpoint" $ do
      api_version <- getLBS "/api/v1/status" >>= okCBOR >>= statusResonse
      unless (api_version == specVersion || api_version == "unversioned") $
        assertFailure $ "ic_api_version should be " ++ show specVersion ++
                        " or \"unversioned\", not " ++ show api_version

  , testCase "spec compliance claimed" $ do
      api_version <- getLBS "/api/v1/status" >>= okCBOR >>= statusResonse

      unless (api_version == specVersion) $
        assertFailure $ "ic_api_version is not " ++ show specVersion ++ ", so no spec compliance assumed"

      -- The system claims to be spec-compliant.
      -- So lets make sure the error code of ic-ref-test reflects reality
      primeTestSuite

  , testGroup "create_canister"
    [ testCase "no id given" $ do
        cid <- submitCBOR >=> createReply $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          ]
        assertBool "opaque id expected" $ isOpaqueId cid
    , testCaseSteps "desired id" $ \step -> do
        step "Creating"
        bytes <- getRand8Bytes
        let id = mkDerivedId defaultUser bytes
        id' <- submitCBOR >=> createReply $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          , "desired_id" =: GBlob id
          ]
        id' @?= id

        step "Creating again"
        submitCBOR >=> statusReject 3 $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          , "desired_id" =: GBlob id
          ]

    , testCase "desired id (wrong class)" $ do
        id <- mkOpaqueId <$> getRand8Bytes
        submitCBOR >=> statusReject 3 $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          , "desired_id" =: GBlob id
          ]

    , testCase "desired id (wrong user)" $ do
        bytes <- getRand8Bytes
        let id = mkDerivedId otherUser bytes
        submitCBOR >=> statusReject 3 $ rec
          [ "request_type" =: GText "create_canister"
          , "sender" =: GBlob defaultUser
          , "desired_id" =: GBlob id
          ]
    , testCase "create_canister necessary" $
        submitCBOR minimalInstall >>= statusReject 3
    ]

  , testGroup "install"
    [ testGroup "required fields" $
        omitFields (envelope defaultSK minimalInstall) $ \req ->
          postCBOR "/api/v1/submit" req >>= code4xx

    , testCaseSteps "trivial wasm module" $ \step -> do
        step "Create"
        can_id <- create

        step "Reinstall fails"
        submitCBOR >=> statusReject 3 $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob trivialWasmModule
          , "arg" =: GBlob ""
          , "mode" =: GText "reinstall"
          ]


        step "Install"
        submitCBOR >=> emptyReply $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob trivialWasmModule
          , "arg" =: GBlob ""
          ]

        step "Install again fails"
        submitCBOR >=> statusReject 3 $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob trivialWasmModule
          , "arg" =: GBlob ""
          , "mode" =: GText "install" -- NB: This is the default
          ]

        step "Reinstall"
        submitCBOR >=> statusReply >=> emptyRec $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob trivialWasmModule
          , "arg" =: GBlob ""
          , "mode" =: GText "reinstall"
          ]

        step "Upgrade"
        submitCBOR >=> emptyReply $ rec
          [ "request_type" =: GText "install_code"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob can_id
          , "module" =: GBlob trivialWasmModule
          , "arg" =: GBlob ""
          , "mode" =: GText "upgrade"
          ]
    ]

  , simpleTestCase "create and install" $ \cid ->
      assertBool "opaque id expected" $ isOpaqueId cid

  , testGroup "simple calls"
    [ simpleTestCase "Call" $ \cid -> do
      r <- call cid $ replyData "ABCD"
      r @?= "ABCD"

    , simpleTestCase "Call (query)" $ \cid -> do
      r <- query cid $ replyData "ABCD"
      r @?= "ABCD"

    , simpleTestCase "Call no non-existant update method" $ \cid ->
      submitCBOR >=> statusReject 3 $ rec
          [ "request_type" =: GText "call"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "no_such_update"
          , "arg" =: GBlob ""
          ]

    , simpleTestCase "Call no non-existant query method" $ \cid ->
      readCBOR >=> statusReject 3 $ rec
          [ "request_type" =: GText "query"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "no_such_update"
          , "arg" =: GBlob ""
          ]

    , simpleTestCase "reject" $ \cid ->
      call' cid >=> statusReject 4 $ reject "ABCD"

    , simpleTestCase "reject (query)" $ \cid ->
      query' cid >=> statusReject 4 $ reject "ABCD"

    , simpleTestCase "No reply" $ \cid ->
      call' cid >=> statusReject 5 $ noop

    , simpleTestCase "No reply (query)" $ \cid ->
      query' cid >=> statusReject 5 $ noop

    , simpleTestCase "Double reply" $ \cid ->
      call' cid >=> statusReject 5 $ reply >>> reply

    , simpleTestCase "Double reply (query)" $ \cid ->
      query' cid >=> statusReject 5 $ reply >>> reply

    , simpleTestCase "Reply data append after reply" $ \cid ->
      call' cid >=> statusReject 5 $ reply >>> replyDataAppend "foo"

    , simpleTestCase "Reply data append after reject" $ \cid ->
      call' cid >=> statusReject 5 $ reject "bar" >>> replyDataAppend "foo"

    , simpleTestCase "Caller" $ \cid -> do
      r <- call cid $ replyData caller
      r @?= defaultUser

    , simpleTestCase "Caller (query)" $ \cid -> do
      r <- query cid $ replyData caller
      r @?= defaultUser
  ]

  , testGroup "state"
    [ simpleTestCase "set/get" $ \cid -> do
      r <- call cid $ setGlobal "FOO" >>> reply
      r @?= ""
      r <- query cid $ replyData getGlobal
      r @?= "FOO"

    , simpleTestCase "set/set/get" $ \cid -> do
      r <- call cid $ setGlobal "FOO" >>> reply
      r @?= ""
      r <- call cid $ setGlobal "BAR" >>> reply
      r @?= ""
      r <- query cid $ replyData getGlobal
      r @?= "BAR"

    , simpleTestCase "resubmission" $ \cid -> do
      -- Submits the same request (same nonce) twice, checks that
      -- the IC does not act twice.
      -- (Using growing stable memory as non-idempotent action)
      r <- callTwice' cid >=> callReply $ ignore (stableGrow (int 1)) >>> reply
      r @?= ""
      r <- query cid $ replyData (i2b stableSize)
      r @?= "\1\0\0\0"

    ]

  , simpleTestCase "self" $ \cid -> do
    r <- query cid $ replyData self
    r @?= cid

  , testGroup "inter-canister calls"
    [ simpleTestCase "to nonexistant canister" $ \cid -> do
      r <- call cid $ inter_call "foo" "bar" defArgs
      BS.take 4 r @?= "\x03\x0\x0\x0"

    , simpleTestCase "to nonexistant method" $ \cid -> do
      r <- call cid $ inter_call cid "bar" defArgs
      BS.take 4 r @?= "\x03\x0\x0\x0"

    , simpleTestCase "Call from query traps (in update)" $ \cid ->
      callToQuery' cid >=> statusReject 5 $ inter_update cid defArgs

    , simpleTestCase "Call from query traps (in query)" $ \cid ->
      query' cid >=> statusReject 5 $ inter_update cid defArgs

    , simpleTestCase "Call from query traps (in inter-canister-call)" $ \cid -> do
      r <- call cid $ inter_call cid "query" defArgs {
        other_side = inter_query cid defArgs
      }
      BS.take 4 r @?= "\x05\x0\x0\x0"

    , simpleTestCase "Self-call (to update)" $ \cid -> do
      r <- call cid $ inter_update cid defArgs
      r @?= ("Hello " <> cid <> " this is " <> cid)

    , simpleTestCase "Self-call (to query)" $ \cid -> do
      r <- call cid $ inter_query cid defArgs
      r @?= ("Hello " <> cid <> " this is " <> cid)

    , simpleTestCase "update commits" $ \cid -> do
      r <- call cid $
        setGlobal "FOO" >>>
        inter_update cid defArgs{ other_side = setGlobal "BAR" >>> reply }
      r @?= ""

      r <- query cid $ replyData getGlobal
      r @?= "BAR"

    , simpleTestCase "query does not commit" $ \cid -> do
      r <- call cid $
        setGlobal "FOO" >>>
        inter_query cid defArgs{ other_side = setGlobal "BAR" >>> reply }
      r @?= ""

      r <- query cid $ replyData getGlobal
      r @?= "FOO"

    , simpleTestCase "query no reply" $ \cid -> do
      r <- call cid $ inter_query cid defArgs{ other_side = noop }
      BS.take 4 r @?= "\5\0\0\0"

    , simpleTestCase "query double reply" $ \cid -> do
      r <- call cid $ inter_query cid defArgs{ other_side = reply >>> reply }
      BS.take 4 r @?= "\5\0\0\0"

    , simpleTestCase "Reject code is 0 in reply" $ \cid -> do
      r <- call cid $ inter_query cid defArgs{ on_reply = replyData (i2b reject_code) }
      r @?= "\x0\x0\x0\x0"

    , simpleTestCase "traps in reply: getting reject message" $ \cid ->
      call' cid >=> statusReject 5 $
        inter_query cid defArgs{ on_reply = replyRejectData }

    , simpleTestCase "traps in reply: getting caller" $ \cid ->
      call' cid >=> statusReject 5 $
        inter_query cid defArgs{ on_reply = replyData caller }

    , simpleTestCase "traps in reject: getting argument" $ \cid ->
      call' cid >=> statusReject 5 $
        inter_query cid defArgs{
          on_reject = replyArgData,
          other_side = reject "rejecting"
        }

    , simpleTestCase "traps in reject: getting caller" $ \cid ->
      call' cid >=> statusReject 5 $
        inter_query cid defArgs{
          on_reject = replyData caller,
          other_side = reject "rejecting"
        }

    , simpleTestCase "Second reply in callback" $ \cid -> do
      r <- call cid $
        setGlobal "FOO" >>>
        replyData "First reply" >>>
        inter_query cid defArgs{
          on_reply = setGlobal "BAR" >>> replyData "Second reply",
          on_reject = setGlobal "BAZ" >>> replyRejectData
        }
      r @?= "First reply"

      -- now check that the callback trapped and did not actual change the global
      -- This check is maybe not as good as we want: There is no guarantee
      -- that the IC actually tried to process the reply message before we do
      -- this query.
      r <- query cid $ replyData getGlobal
      r @?= "FOO"

    , simpleTestCase "partial reply" $ \cid -> do
      r <- call cid $
        replyDataAppend "FOO" >>>
        inter_query cid defArgs{ on_reply = replyDataAppend "BAR" >>> reply }
      r @?= "BAR" -- check that the FOO is silently dropped

    , testGroup "two callbacks"
      [ simpleTestCase "reply after trap" $ \cid -> do
        r <- call cid $
          inter_query cid defArgs{ on_reply = trap "first callback traps" } >>>
          inter_query cid defArgs{ on_reply = replyData "good" }
        r @?= "good"


      , simpleTestCase "trap after reply" $ \cid -> do
        r <- call cid $
          inter_query cid defArgs{ on_reply = replyData "good" } >>>
          inter_query cid defArgs{ on_reply = trap "second callback traps" }
        r @?= "good"

      , simpleTestCase "both trap" $ \cid ->
        call' cid >=> statusReject 5 $
          inter_query cid defArgs{ on_reply = trap "first callback traps" } >>>
          inter_query cid defArgs{ on_reply = trap "second callback traps" }
      ]

    , simpleTestCase "Call to other canister (to update)" $ \cid -> do
      cid2 <- install noop
      r <- call cid $ inter_update cid2 defArgs
      r @?= ("Hello " <> cid <> " this is " <> cid2)

    , simpleTestCase "Call to other canister (to query)" $ \cid -> do
      cid2 <- install noop
      r <- call cid $ inter_query cid2 defArgs
      r @?= ("Hello " <> cid <> " this is " <> cid2)
    ]

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

  , testGroup "upgrades" $
    let installForUpgrade on_pre_upgrade = install $
            setGlobal "FOO" >>>
            ignore (stableGrow (int 1)) >>>
            stableWrite (int 0) "BAR______" >>>
            onPreUpgrade (callback on_pre_upgrade)

        checkNoUpgrade cid = do
          r <- query cid $ replyData getGlobal
          r @?= "FOO"
          r <- query cid $ replyData (stableRead (int 0) (int 9))
          r @?= "BAR______"
    in
    [ testCase "succeeding" $ do
      -- check that the pre-upgrade hook has access to the old state
      cid <- installForUpgrade $ stableWrite (int 3) getGlobal
      checkNoUpgrade cid

      upgrade' cid >=> emptyReply $ stableWrite (int 6) (stableRead (int 0) (int 3))

      r <- query cid $ replyData getGlobal
      r @?= ""
      r <- query cid $ replyData (stableRead (int 0) (int 9))
      r @?= "BARFOOBAR"

    , testCase "trapping in pre-upgrade" $ do
      cid <- installForUpgrade $ trap "trap in pre-upgrade"
      checkNoUpgrade cid

      upgrade' cid >=> statusReject 5 $ noop
      checkNoUpgrade cid

    , testCase "trapping in pre-upgrade (by calling)" $ do
      cid <- installForUpgrade $ trap "trap in pre-upgrade"
      r <- call cid $
        reply >>>
        onPreUpgrade (callback (
            call_simple
                (bytes cid)
                "query"
                (callback replyArgData)
                (callback replyRejectData)
                (callback noop)
        ))
      r @?= ""
      checkNoUpgrade cid

      upgrade' cid >=> statusReject 5 $ noop
      checkNoUpgrade cid

    , testCase "trapping in pre-upgrade (by accessing arg)" $ do
      cid <- installForUpgrade $ ignore argData
      checkNoUpgrade cid

      upgrade' cid >=> statusReject 5 $ noop
      checkNoUpgrade cid

    , testCase "trapping in post-upgrade" $ do
      cid <- installForUpgrade $ stableWrite (int 3) getGlobal
      checkNoUpgrade cid

      upgrade' cid >=> statusReject 5 $ trap "trap in post-upgrade"
      checkNoUpgrade cid

    , testCase "trapping in post-upgrade (by calling)" $ do
      cid <- installForUpgrade $ stableWrite (int 3) getGlobal
      checkNoUpgrade cid

      upgrade' cid >=> statusReject 5 $
        call_simple
            (bytes cid)
            "query"
            (callback replyArgData)
            (callback replyRejectData)
            (callback noop)
      checkNoUpgrade cid
    ]

  , testGroup "reinstall" $
    let reinstall :: HasCallStack => Blob -> Prog -> IO GenR
        reinstall cid prog = do
          universal_wasm <- getTestWasm "universal_canister"
          submitCBOR $ rec
            [ "request_type" =: GText "install_code"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid
            , "module" =: GBlob universal_wasm
            , "arg" =: GBlob (run prog)
            , "mode" =: GText "reinstall"
            ]
    in
    [ testCase "succeeding" $ do
      cid <- install $
            setGlobal "FOO" >>>
            ignore (stableGrow (int 1)) >>>
            stableWrite (int 0) "FOO______"
      r <- query cid $ replyData getGlobal
      r @?= "FOO"
      r <- query cid $ replyData (stableRead (int 0) (int 9))
      r @?= "FOO______"
      r <- query cid $ replyData (i2b stableSize)
      r @?= "\1\0\0\0"

      reinstall cid >=> emptyReply $
            setGlobal "BAR" >>>
            ignore (stableGrow (int 2)) >>>
            stableWrite (int 0) "BAR______"

      r <- query cid $ replyData getGlobal
      r @?= "BAR"
      r <- query cid $ replyData (stableRead (int 0) (int 9))
      r @?= "BAR______"
      r <- query cid $ replyData (i2b stableSize)
      r @?= "\2\0\0\0"

    , testCase "trapping" $ do
      cid <- install $
            setGlobal "FOO" >>>
            ignore (stableGrow (int 1)) >>>
            stableWrite (int 0) "FOO______"
      r <- query cid $ replyData getGlobal
      r @?= "FOO"
      r <- query cid $ replyData (stableRead (int 0) (int 9))
      r @?= "FOO______"
      r <- query cid $ replyData (i2b stableSize)
      r @?= "\1\0\0\0"

      reinstall cid >=> statusReject 5 $ trap "Trapping the reinstallation"

      r <- query cid $ replyData getGlobal
      r @?= "FOO"
      r <- query cid $ replyData (stableRead (int 0) (int 9))
      r @?= "FOO______"
      r <- query cid $ replyData (i2b stableSize)
      r @?= "\1\0\0\0"
    ]

  , testGroup "debug facilities"
    [ simpleTestCase "Using debug_print" $ \cid -> do
      r <- call cid $ debugPrint "ic-ref-test print" >>> reply
      r @?= ""

    , simpleTestCase "Using debug_print (query)" $ \cid -> do
      r <- query cid $ debugPrint "ic-ref-test print" >>> reply
      r @?= ""

    , simpleTestCase "Using debug_print with invalid bounds" $ \cid -> do
      r <- query cid $ badPrint >>> reply
      r @?= ""

    , simpleTestCase "Explicit trap" $ \cid -> do
      call' cid >=> statusReject 5 $ trap "trapping"

    , simpleTestCase "Explicit trap (query)" $ \cid -> do
      query' cid >=> statusReject 5 $ trap "trapping"
    ]

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
        cid <- create
        install' cid pgm >>= statusReject 5
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

  , testGroup "query"
    [ testGroup "required fields" $
        omitFields (envelope defaultSK queryToNonExistant) $ \req ->
          postCBOR "/api/v1/read" req >>= code4xx

    , testCase "non-existing canister" $
        postCBOR "/api/v1/read" (envelope defaultSK queryToNonExistant)
          >>= okCBOR >>= statusReject 3
    ]

  , testGroup "request_status"
    [ testGroup "required fields" $
        omitFields (envelope defaultSK requestStatusNonExistant) $ \req ->
          postCBOR "/api/v1/read" req >>= code4xx
    ]

  , testGroup "signature checking" $
    [ ("with bad signature", badEnvelope)
    , ("with wrong key", envelope otherSK)
    ] <&> \(name, env) -> testGroup name
      [ simpleTestCase "in query" $ \cid -> do
        let query = rec
              [ "request_type" =: GText "query"
              , "sender" =: GBlob defaultUser
              , "canister_id" =: GBlob cid
              , "method_name" =: GText "query"
              , "arg" =: GBlob (run reply)
              ]
        r <- readCBOR query >>= callReply
        r @?= ""
        postCBOR "/api/v1/read" (env query) >>= code4xx

      , testCase "in unknown request status" $
          postCBOR "/api/v1/read" (env requestStatusNonExistant) >>= code4xx

      , testCase "in request status" $ do
          req <- addNonce $ rec
                [ "request_type" =: GText "create_canister"
                , "sender" =: GBlob defaultUser
                ]
          _cid <- submitCBOR req >>= createReply
          let status_req = rec
                [ "request_type" =: GText "request_status"
                , "request_id" =: GBlob (requestId req)
                ]
          postCBOR "/api/v1/read" (envelope defaultSK status_req) >>= code2xx
          postCBOR "/api/v1/read" (env status_req) >>= code4xx
      , testCase "in install" $ do
          cid <- create
          req <- addNonce $ rec
                [ "request_type" =: GText "install_code"
                , "sender" =: GBlob defaultUser
                , "canister_id" =: GBlob cid
                , "module" =: GBlob trivialWasmModule
                , "arg" =: GBlob ""
                ]
          postCBOR "/api/v1/submit" (env req) >>= code202_or_4xx

          -- Also check that the request was not created
          ingressDelay
          let status_req = rec
                [ "request_type" =: GText "request_status"
                , "request_id" =: GBlob (requestId req)
                ]
          postCBOR "/api/v1/read" (envelope defaultSK status_req) >>= code4xx_or_unknown

      , simpleTestCase "in call" $ \cid -> do
          req <- addNonce $ rec
                [ "request_type" =: GText "call"
                , "sender" =: GBlob defaultUser
                , "canister_id" =: GBlob cid
                , "method_name" =: GText "query"
                , "arg" =: GBlob (run reply)
                ]
          postCBOR "/api/v1/submit" (env req) >>= code202_or_4xx

          -- Also check that the request was not created
          ingressDelay
          let status_req = rec
                [ "request_type" =: GText "request_status"
                , "request_id" =: GBlob (requestId req)
                ]
          postCBOR "/api/v1/read" (envelope defaultSK status_req) >>= code4xx_or_unknown

          -- check that with a valid signature, this would have worked
          r <- submitCBOR req >>= callReply
          r @?= ""
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

getLBS :: (HasCallStack, HasEndpoint) => String -> IO (Response BS.ByteString)
getLBS path = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest $ ep ++ path
    httpLbs request manager
  where
    Endpoint ep = endPoint

-- | Posting a CBOR request, returning a raw bytestring
postCBOR :: (HasCallStack, HasEndpoint) => String -> GenR -> IO (Response BS.ByteString)
postCBOR path gr = do
    -- this uses a fresh manger for each request. If we care about performance,
    -- this could revisited (to share connections)
    manager <- newManager defaultManagerSettings
    request <- parseRequest $ ep ++ path
    request <- return $ request
      { method = "POST"
      , requestBody = RequestBodyLBS $ BS.toLazyByteString $ encode gr
      , requestHeaders = [(hContentType, "application/cbor")]
      }
    httpLbs request manager
  where
    Endpoint ep = endPoint

-- | Add envelope to CBOR request, post to "read", return decoded CBOR
readCBOR :: HasEndpoint => GenR -> IO GenR
readCBOR req = postCBOR "/api/v1/read" (envelope defaultSK req) >>= okCBOR

-- | Add envelope to CBOR, and a nonce if not there, post to "submit", poll for
-- the request response, and return decoded CBOR
submitCBOR :: HasEndpoint => GenR -> IO GenR
submitCBOR req = do
  req <- addNonce req
  res <- postCBOR "/api/v1/submit" (envelope defaultSK req)
  code202 res
  assertBool "Response body not empty" (BS.null (responseBody res))
  awaitStatus (requestId req)

-- | Submits twice
submitCBORTwice :: HasEndpoint => GenR -> IO GenR
submitCBORTwice req = do
  req <- addNonce req
  res <- postCBOR "/api/v1/submit" (envelope defaultSK req)
  code202 res
  res <- postCBOR "/api/v1/submit" (envelope defaultSK req)
  code202 res
  assertBool "Response body not empty" (BS.null (responseBody res))
  awaitStatus (requestId req)

create :: HasEndpoint => IO Blob
create = submitCBOR >=> createReply $ rec
  [ "request_type" =: GText "create_canister"
  , "sender" =: GBlob defaultUser
  ]

awaitStatus :: HasEndpoint => Blob -> IO GenR
awaitStatus rid = loop $ do
    pollDelay
    res <- postCBOR "/api/v1/read" $ envelope defaultSK $ rec
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
ingressDelay = threadDelay $ 2 * 1000 * 1000 -- 2 seconds


-- * HTTP Response predicates

codePred :: HasCallStack => String -> (Int -> Bool) -> Response Blob -> IO ()
codePred expt pred response = assertBool
    ("Status " ++ show c ++ " is not " ++ expt ++ "\n" ++ msg)
    (pred c)
  where
    c = statusCode (responseStatus response)
    msg = T.unpack (T.decodeUtf8With T.lenientDecode (BS.toStrict (BS.take 200 (responseBody response))))

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

statusUnknown :: HasCallStack => GenR -> IO ()
statusUnknown = record $ do
    s <- field text "status"
    lift $ s @?= "unknown"

statusReject :: HasCallStack => Natural -> GenR -> IO ()
statusReject code = record $ do
  s <- field text "status"
  lift $ s @?= "rejected"
  n <- field nat "reject_code"
  msg <- field text "reject_message"
  lift $ assertBool
    ("Reject code " ++ show n ++ " is not " ++ show code ++ "\n" ++ T.unpack msg)
    (n == code)

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
    swallowAllFields -- More fields are explicitly allowed
    return v

code4xx_or_unknown :: HasCallStack => Response Blob -> IO ()
code4xx_or_unknown response
  | 200 == c = okCBOR response >>= statusUnknown
  | 400 <= c && c < 500 = return ()
  | otherwise = assertFailure $
      "Status " ++ show c ++ " is not 4xx or status unknown\n" ++ msg
 where
  c = statusCode (responseStatus response)
  msg = T.unpack (T.decodeUtf8With T.lenientDecode (BS.toStrict (BS.take 200 (responseBody response))))

-- * Interacting with the universal canister

-- Some common operations on the universal canister
-- The primed variant (call') return the response record,
-- e.g. to check for error conditions.
-- The unprimed variant expect a reply.

install' :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO GenR
install' cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  submitCBOR $ rec
    [ "request_type" =: GText "install_code"
    , "sender" =: GBlob defaultUser
    , "canister_id" =: GBlob cid
    , "module" =: GBlob universal_wasm
    , "arg" =: GBlob (run prog)
    ]

-- Also calls create
install :: (HasCallStack, HasEndpoint) => Prog -> IO Blob
install prog = do
    cid <- create
    install' cid prog >>= emptyReply
    return cid

upgrade' :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO GenR
upgrade' cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  submitCBOR $ rec
    [ "request_type" =: GText "install_code"
    , "sender" =: GBlob defaultUser
    , "canister_id" =: GBlob cid
    , "module" =: GBlob universal_wasm
    , "arg" =: GBlob (run prog)
    , "mode" =: GText "upgrade"
    ]

callRequest :: HasEndpoint => Blob -> Prog -> GenR
callRequest cid prog = rec
    [ "request_type" =: GText "call"
    , "sender" =: GBlob defaultUser
    , "canister_id" =: GBlob cid
    , "method_name" =: GText "update"
    , "arg" =: GBlob (run prog)
    ]

callToQuery' :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO GenR
callToQuery' cid prog = submitCBOR $ rec
    [ "request_type" =: GText "call"
    , "sender" =: GBlob defaultUser
    , "canister_id" =: GBlob cid
    , "method_name" =: GText "query"
    , "arg" =: GBlob (run prog)
    ]

call' :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO GenR
call' cid prog = submitCBOR (callRequest cid prog)

callTwice' :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO GenR
callTwice' cid prog = submitCBORTwice (callRequest cid prog)

call :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO Blob
call cid prog = call' cid prog >>= callReply

query' :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO GenR
query' cid prog =
  readCBOR $ rec
    [ "request_type" =: GText "query"
    , "sender" =: GBlob defaultUser
    , "canister_id" =: GBlob cid
    , "method_name" =: GText "query"
    , "arg" =: GBlob (run prog)
    ]

query :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO Blob
query cid prog = query' cid prog >>= callReply

-- Shortcut for test cases that just need one canister
simpleTestCase :: HasEndpoint => String -> (Blob -> IO ()) -> TestTree
simpleTestCase name act = testCase name $ install noop >>= act


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

-- * Endpoint handling

-- Yes, implicit arguments are frowned upon. But they are also very useful.

type HasEndpoint = (?endPoint :: Endpoint)

withEndPoint :: (forall. HasEndpoint => TestTree) -> TestTree
withEndPoint act = askOption $ \ep -> let ?endPoint = (ep :: Endpoint) in act

endPoint :: HasEndpoint => Endpoint
endPoint = ?endPoint

-- * Test data access

getTestFile :: FilePath -> IO FilePath
getTestFile file =
    lookupEnv "IC_TEST_DATA" >>= \case
    Just fp -> return $ fp </> file
    Nothing -> do
      -- nix use
      exePath <- getExecutablePath
      let exeRelPath = takeDirectory exePath </> "../test-data"
      -- convenient for cabal new-run use
      try [ exeRelPath, "test-data", "../test-data", "impl/test-data" ]
  where
    try (d:ds) = doesFileExist (d </> file) >>= \case
      True -> return (d </> file)
      False -> try ds
    try [] = error $ "getTestDir: Could not read " ++ file ++ " from test-data/. Please consult impl/README.md"

getTestWasm :: FilePath -> IO BS.ByteString
getTestWasm base = do
  fp <- getTestFile $ base <.> "wasm"
  BS.readFile fp
