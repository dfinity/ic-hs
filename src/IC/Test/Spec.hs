{- |

This module contains a test suite for the Internet Computer

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
import Data.Word
import Data.Functor
import GHC.TypeLits
import System.FilePath
import System.Directory
import System.Environment
import System.Random
import Codec.Candid (Principal(..))
import qualified Codec.Candid as Candid
import Data.Row
import qualified Data.Row.Variants as V

import IC.Version
import IC.HTTP.GenR
import IC.HTTP.GenR.Parse
import IC.HTTP.CBOR (decode, encode)
import IC.HTTP.RequestId
import IC.Management
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

addNonce :: GenR -> IO GenR
addNonce (GRec hm) | "nonce" `HM.member` hm = return (GRec hm)
addNonce (GRec hm) = do
  nonce <- getRand8Bytes
  return $ GRec $ HM.insert "nonce" (GBlob nonce) hm
addNonce r = return r

envelope :: SecretKey -> GenR -> GenR
envelope sk content = rec
    [ "sender_pubkey" =: GBlob (toPublicKey sk)
    , "sender_sig" =: GBlob (sign "\x0Aic-request" sk (requestId content))
    , "content" =: content
    ]

-- a little bit of smartness in our combinators
chooseKey :: GenR -> SecretKey
chooseKey (GRec hm)
  | Just (GBlob id) <- HM.lookup "sender" hm
  , id == defaultUser
  = defaultSK
  | Just (GBlob id) <- HM.lookup "sender" hm
  , id == otherUser
  = otherSK
chooseKey _ = defaultSK

badEnvelope :: GenR -> GenR
badEnvelope content = rec
    [ "sender_pubkey" =: GBlob (toPublicKey defaultSK)
    , "sender_sig" =: GBlob (BS.replicate 64 0x42)
    , "content" =: content
    ]

noDomainSepEnv :: SecretKey -> GenR -> GenR
noDomainSepEnv sk content = rec
    [ "sender_pubkey" =: GBlob (toPublicKey sk)
    , "sender_sig" =: GBlob (sign "" sk (requestId content))
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

  , simpleTestCase "create and install" $ \_ ->
      return ()

  , testCase "create_canister necessary" $
      ic_install' ic00 (enum #install) doesn'tExist trivialWasmModule ""
          >>= statusReject [3,5]

  , testCaseSteps "management requests" $ \step -> do
      step "Create"
      can_id <- ic_create ic00

      step "Install"
      ic_install ic00 (enum #install) can_id trivialWasmModule ""

      step "Install again fails"
      ic_install' ic00 (enum #install) can_id trivialWasmModule ""
        >>= statusReject [3,5]

      step "Reinstall"
      ic_install ic00 (enum #reinstall) can_id trivialWasmModule ""

      step "Reinstall as wrong user"
      ic_install' (ic00as otherUser) (enum #reinstall) can_id trivialWasmModule ""
        >>= statusReject [3,5]

      step "Upgrade"
      ic_install ic00 (enum #upgrade) can_id trivialWasmModule ""

      step "Change controller"
      ic_set_controller ic00 can_id otherUser

      step "Change controller (with wrong controller)"
      ic_set_controller' ic00 can_id otherUser
        >>= statusReject [3,5]

      step "Reinstall as new controller"
      ic_install (ic00as otherUser) (enum #reinstall) can_id trivialWasmModule ""

  , testCaseSteps "reinstall on empty" $ \step -> do
      step "Create"
      can_id <- ic_create ic00

      step "Reinstall over empty canister"
      ic_install ic00 (enum #reinstall) can_id trivialWasmModule ""

  , testCaseSteps "canister lifecycle" $ \step -> do
      cid <- install noop

      step "Is running?"
      sv <- ic_canister_status ic00 cid
      V.view #running sv @?= Just ()

      step "Stop"
      ic_stop_canister ic00 cid

      step "Is stopped?"
      sv <- ic_canister_status ic00 cid
      V.view #stopped sv @?= Just ()

      step "Stop is noop"
      ic_stop_canister ic00 cid

      step "Cannot call (update)?"
      call' cid >=> statusReject [5] $ reply

      step "Cannot call (query)?"
      query' cid >=> statusReject [5] $ reply

      step "Start canister"
      ic_start_canister ic00 cid

      step "Is running?"
      sv <- ic_canister_status ic00 cid
      V.view #running sv @?= Just ()

      step "Can call (update)?"
      r <- call cid $ reply
      r @?= ""

      step "Can call (query)?"
      r <- query cid $ reply
      r @?= ""

      step "Start is noop"
      ic_start_canister ic00 cid

  , testCaseSteps "canister deletion" $ \step -> do
      cid <- install noop

      step "Deletion fails"
      ic_delete_canister' ic00 cid >>= statusReject [5]

      step "Stop"
      ic_stop_canister ic00 cid

      step "Is stopped?"
      sv <- ic_canister_status ic00 cid
      V.view #stopped sv @?= Just ()

      step "Deletion succeeds"
      ic_delete_canister ic00 cid

      step "Cannot call (update)?"
      call' cid >=> statusReject [3] $ reply

      step "Cannot call (query)?"
      query' cid >=> statusReject [3] $ reply

      step "Cannot query canister status"
      ic_canister_status' ic00 cid >>= statusReject [3,5]

      step "Deletion fails"
      ic_delete_canister' ic00 cid >>= statusReject [3,5]


  , testCaseSteps "canister lifecycle (wrong controller)" $ \step -> do
      cid <- install noop

      step "Start as wrong user"
      ic_start_canister' (ic00as otherUser) cid >>= statusReject [3,5]
      step "Stop as wrong user"
      ic_stop_canister' (ic00as otherUser) cid >>= statusReject [3,5]
      step "Canister Status as wrong user"
      ic_canister_status' (ic00as otherUser) cid >>= statusReject [3,5]
      step "Delete as wrong user"
      ic_delete_canister' (ic00as otherUser) cid >>= statusReject [3,5]


  , testCaseSteps "aaaaa-aa (inter-canister)" $ \step -> do
    -- install universal canisters to proxy the requests
    cid <- install noop
    cid2 <- install noop

    step "Create"
    can_id <- ic_create (ic00via cid)

    step "Install"
    ic_install (ic00via cid) (enum #install) can_id trivialWasmModule ""

    step "Install again fails"
    ic_install' (ic00via cid) (enum #install) can_id trivialWasmModule ""
      >>= statusRelayReject [3,5]

    step "Reinstall"
    ic_install (ic00via cid) (enum #reinstall) can_id trivialWasmModule ""

    step "Reinstall as wrong user"
    ic_install' (ic00via cid2) (enum #reinstall) can_id trivialWasmModule ""
      >>= statusRelayReject [3,5]

    step "Upgrade"
    ic_install (ic00via cid) (enum #upgrade) can_id trivialWasmModule ""

    step "Change controller"
    ic_set_controller (ic00via cid) can_id cid2

    step "Change controller (with wrong controller)"
    ic_set_controller' (ic00via cid) can_id cid2
      >>= statusRelayReject [3,5]

    step "Reinstall as new controller"
    ic_install (ic00via cid2) (enum #reinstall) can_id trivialWasmModule ""

    step "Create"
    can_id2 <- ic_create (ic00via cid)

    step "Reinstall on empty"
    ic_install (ic00via cid) (enum #reinstall) can_id2 trivialWasmModule ""

  , simpleTestCase "randomness" $ \cid -> do
    r1 <- ic_raw_rand ic00
    r2 <- ic_raw_rand ic00
    r3 <- ic_raw_rand (ic00via cid)
    BS.length r1 @?= 32
    BS.length r2 @?= 32
    BS.length r3 @?= 32
    assertBool "random blobs are different" $ r1 /= r2
    assertBool "random blobs are different" $ r1 /= r3
    assertBool "random blobs are different" $ r2 /= r3

  , testGroup "simple calls"
    [ simpleTestCase "Call" $ \cid -> do
      r <- call cid $ replyData "ABCD"
      r @?= "ABCD"

    , simpleTestCase "Call (query)" $ \cid -> do
      r <- query cid $ replyData "ABCD"
      r @?= "ABCD"

    , simpleTestCase "Call no non-existant update method" $ \cid ->
      submitCBOR >=> statusReject [3] $ rec
          [ "request_type" =: GText "call"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "no_such_update"
          , "arg" =: GBlob ""
          ]

    , simpleTestCase "Call no non-existant query method" $ \cid ->
      readCBOR >=> statusReject [3] $ rec
          [ "request_type" =: GText "query"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "no_such_update"
          , "arg" =: GBlob ""
          ]

    , simpleTestCase "reject" $ \cid ->
      call' cid >=> statusReject [4] $ reject "ABCD"

    , simpleTestCase "reject (query)" $ \cid ->
      query' cid >=> statusReject [4] $ reject "ABCD"

    , simpleTestCase "No reply" $ \cid ->
      call' cid >=> statusReject [5] $ noop

    , simpleTestCase "No reply (query)" $ \cid ->
      query' cid >=> statusReject [5] $ noop

    , simpleTestCase "Double reply" $ \cid ->
      call' cid >=> statusReject [5] $ reply >>> reply

    , simpleTestCase "Double reply (query)" $ \cid ->
      query' cid >=> statusReject [5] $ reply >>> reply

    , simpleTestCase "Reply data append after reply" $ \cid ->
      call' cid >=> statusReject [5] $ reply >>> replyDataAppend "foo"

    , simpleTestCase "Reply data append after reject" $ \cid ->
      call' cid >=> statusReject [5] $ reject "bar" >>> replyDataAppend "foo"

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
    [ simpleTestCase "to nonexistant canister" $ \cid ->
      call' cid >=> statusRelayReject [3] $ inter_call "foo" "bar" defArgs

    , simpleTestCase "to nonexistant method" $ \cid -> do
      call' cid >=> statusRelayReject [3] $ inter_call cid "bar" defArgs

    , simpleTestCase "Call from query traps (in update)" $ \cid ->
      callToQuery' cid >=> statusReject [5] $ inter_update cid defArgs

    , simpleTestCase "Call from query traps (in query)" $ \cid ->
      query' cid >=> statusReject [5] $ inter_update cid defArgs

    , simpleTestCase "Call from query traps (in inter-canister-call)" $ \cid ->
      call' cid >=> statusRelayReject [5] $ inter_call cid "query" defArgs {
        other_side = inter_query cid defArgs
      }

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

    , simpleTestCase "query no reply" $ \cid ->
      call' cid >=> statusRelayReject [5] $
        inter_query cid defArgs{ other_side = noop }

    , simpleTestCase "query double reply" $ \cid ->
      call' cid >=> statusRelayReject [5] $
        inter_query cid defArgs{ other_side = reply >>> reply }

    , simpleTestCase "Reject code is 0 in reply" $ \cid -> do
      call' cid >=> statusRelayReject [0] $
        inter_query cid defArgs{ on_reply = replyData (i2b reject_code) }

    , simpleTestCase "traps in reply: getting reject message" $ \cid ->
      call' cid >=> statusReject [5] $
        inter_query cid defArgs{ on_reply = replyRejectData }

    , simpleTestCase "traps in reply: getting caller" $ \cid ->
      call' cid >=> statusReject [5] $
        inter_query cid defArgs{ on_reply = replyData caller }

    , simpleTestCase "traps in reject: getting argument" $ \cid ->
      call' cid >=> statusReject [5] $
        inter_query cid defArgs{
          on_reject = replyArgData,
          other_side = reject "rejecting"
        }

    , simpleTestCase "traps in reject: getting caller" $ \cid ->
      call' cid >=> statusReject [5] $
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
        call' cid >=> statusReject [5] $
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
    call' cid >=> statusReject [5] $ stableWrite (int 0) "FOO"
    step "Set stable mem (failing, query)"
    query' cid >=> statusReject [5] $ stableWrite (int 0) "FOO"

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

  , testGroup "time" $
    let getTimeTwice = cat (i64tob getTime) (i64tob getTime)
        isSameTime blob = do
          assertBool "Time not 8 bytes" (BS.length blob == 2*8)
          assertBool "Time not constant" (BS.take 8 blob == BS.drop 8 blob)
    in
    [ simpleTestCase "in query" $ \cid -> do
      query cid (replyData getTimeTwice) >>= isSameTime
    , simpleTestCase "in update" $ \cid -> do
      query cid (replyData getTimeTwice) >>= isSameTime
    , testCase "in install" $ do
      cid <- install $ setGlobal (getTimeTwice)
      query cid (replyData getGlobal) >>= isSameTime
    , testCase "in pre_upgrade" $ do
      cid <- install $
        ignore (stableGrow (int 1)) >>>
        onPreUpgrade (callback $ stableWrite (int 0) (getTimeTwice))
      query cid (replyData (stableRead (int 0) (int (2*8)))) >>= isSameTime
    , simpleTestCase "in post_upgrade" $ \cid -> do
      upgrade cid $ setGlobal (getTimeTwice)
      query cid (replyData getGlobal) >>= isSameTime
    ]

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

      upgrade cid $ stableWrite (int 6) (stableRead (int 0) (int 3))

      r <- query cid $ replyData getGlobal
      r @?= ""
      r <- query cid $ replyData (stableRead (int 0) (int 9))
      r @?= "BARFOOBAR"

    , testCase "trapping in pre-upgrade" $ do
      cid <- installForUpgrade $ trap "trap in pre-upgrade"
      checkNoUpgrade cid

      upgrade' cid >=> statusReject [5] $ noop
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

      upgrade' cid >=> statusReject [5] $ noop
      checkNoUpgrade cid

    , testCase "trapping in pre-upgrade (by accessing arg)" $ do
      cid <- installForUpgrade $ ignore argData
      checkNoUpgrade cid

      upgrade' cid >=> statusReject [5] $ noop
      checkNoUpgrade cid

    , testCase "trapping in post-upgrade" $ do
      cid <- installForUpgrade $ stableWrite (int 3) getGlobal
      checkNoUpgrade cid

      upgrade' cid >=> statusReject [5] $ trap "trap in post-upgrade"
      checkNoUpgrade cid

    , testCase "trapping in post-upgrade (by calling)" $ do
      cid <- installForUpgrade $ stableWrite (int 3) getGlobal
      checkNoUpgrade cid

      upgrade' cid >=> statusReject [5] $
        call_simple
            (bytes cid)
            "query"
            (callback replyArgData)
            (callback replyRejectData)
            (callback noop)
      checkNoUpgrade cid
    ]

  , testGroup "reinstall"
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

      reinstall cid $
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

      reinstall' cid >=> statusReject [5] $ trap "Trapping the reinstallation"

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
      call' cid >=> statusReject [5] $ trap "trapping"

    , simpleTestCase "Explicit trap (query)" $ \cid -> do
      query' cid >=> statusReject [5] $ trap "trapping"
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
        cid <- ic_create ic00
        install' cid pgm >>= statusReject [5]
        -- canister does not exist
        query' cid >=> statusReject [3] $ noop
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
          >>= okCBOR >>= statusReject [3]
    ]

  , testGroup "request_status"
    [ testGroup "required fields" $
        omitFields (envelope defaultSK requestStatusNonExistant) $ \req ->
          postCBOR "/api/v1/read" req >>= code4xx
    ]

  , testGroup "signature checking" $
    [ ("with bad signature", badEnvelope)
    , ("with wrong key", envelope otherSK)
    , ("with no domain separator", noDomainSepEnv defaultSK)
    ] <&> \(name, env) -> testGroup name
      [ simpleTestCase "in query" $ \cid -> do
        req <- addNonce $ rec
              [ "request_type" =: GText "query"
              , "sender" =: GBlob defaultUser
              , "canister_id" =: GBlob cid
              , "method_name" =: GText "query"
              , "arg" =: GBlob (run reply)
              ]
        r <- readCBOR req >>= callReply
        r @?= ""
        postCBOR "/api/v1/read" (env req) >>= code4xx

      , testCase "in unknown request status" $
          postCBOR "/api/v1/read" (env requestStatusNonExistant) >>= code4xx

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

      , testCase "in request status" $ do
          req <- addNonce $ rec
                [ "request_type" =: GText "call"
                , "sender" =: GBlob defaultUser
                , "canister_id" =: GBlob ""
                , "method_name" =: GText "create_canister"
                , "arg" =: GBlob "DIDL\0\0"
                ]
          _reply <- submitCBOR req >>= callReply
          let status_req = rec
                [ "request_type" =: GText "request_status"
                , "request_id" =: GBlob (requestId req)
                ]
          postCBOR "/api/v1/read" (envelope defaultSK status_req) >>= code2xx
          postCBOR "/api/v1/read" (env status_req) >>= code4xx

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

-- | Add envelope to CBOR request, add a nonce if it is not there,
-- post to "read", return decoded CBOR
readCBOR :: HasEndpoint => GenR -> IO GenR
readCBOR req = do
  req <- addNonce req
  postCBOR "/api/v1/read" (envelope (chooseKey req) req) >>= okCBOR

-- | Add envelope to CBOR, and a nonce if not there, post to "submit", poll for
-- the request response, and return decoded CBOR
submitCBOR :: (HasCallStack, HasEndpoint) => GenR -> IO GenR
submitCBOR req = do
  let key = chooseKey req
  req <- addNonce req
  res <- postCBOR "/api/v1/submit" (envelope key req)
  code202 res
  assertBool "Response body not empty" (BS.null (responseBody res))
  awaitStatus key (requestId req)

-- | Submits twice
submitCBORTwice :: HasEndpoint => GenR -> IO GenR
submitCBORTwice req = do
  let key = chooseKey req
  req <- addNonce req
  res <- postCBOR "/api/v1/submit" (envelope key req)
  code202 res
  res <- postCBOR "/api/v1/submit" (envelope key req)
  code202 res
  assertBool "Response body not empty" (BS.null (responseBody res))
  awaitStatus key (requestId req)

awaitStatus :: HasEndpoint => SecretKey -> Blob -> IO GenR
awaitStatus key rid = loop $ do
    pollDelay
    res <- postCBOR "/api/v1/read" $ envelope key $ rec
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

statusUnknown :: HasCallStack => GenR -> IO ()
statusUnknown = record $ do
    s <- field text "status"
    lift $ s @?= "unknown"

statusReject :: HasCallStack => [Natural] -> GenR -> IO ()
statusReject codes = record $ do
  s <- field text "status"
  lift $ s @?= "rejected"
  n <- field nat "reject_code"
  msg <- field text "reject_message"
  lift $ assertBool
    ("Reject code " ++ show n ++ " not in " ++ show codes ++ "\n" ++ T.unpack msg)
    (n `elem` codes)

statusReply :: HasCallStack => GenR -> IO GenR
statusReply = record $ do
    s <- field text "status"
    case s of
      "replied" -> field anyType "reply"
      "rejected" -> do
        msg <- field text "reject_message"
        code <- field nat "reject_code"
        lift $ assertFailure $ "expected status == \"replied\" but got reject (code " ++ show code ++ "):\n" ++ T.unpack msg
      s -> lift $ assertFailure $ "expected status == \"replied\" but got " ++ show s

-- A reject forwarded by replyRejectData
statusRelayReject :: HasCallStack => [Word32] -> GenR -> IO ()
statusRelayReject codes r = do
  b <- callReply r
  assertBool
    ("Reject code " ++ show b ++ " not in" ++ show codes ++ "\n")
    (BS.take 4 b `elem` map (BS.toLazyByteString . BS.word32LE) codes)

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

-- * Interacting with aaaaa-aa (via HTTP)

-- how to reach the management canister
type IC00 = T.Text -> Blob -> IO GenR

ic00 :: HasEndpoint => IC00
ic00 method_name arg = submitCBOR $ rec
      [ "request_type" =: GText "call"
      , "sender" =: GBlob defaultUser
      , "canister_id" =: GBlob ""
      , "method_name" =: GText method_name
      , "arg" =: GBlob arg
      ]

ic00as :: HasEndpoint => Blob -> IC00
ic00as user method_name arg = submitCBOR $ rec
      [ "request_type" =: GText "call"
      , "sender" =: GBlob user
      , "canister_id" =: GBlob ""
      , "method_name" =: GText method_name
      , "arg" =: GBlob arg
      ]

ic00via :: HasEndpoint => Blob -> IC00
ic00via cid method_name arg =
  call' cid $
    call_simple
        (bytes "") -- aaaaa-aa
        (bytes (BS.fromStrict (T.encodeUtf8 method_name)))
        (callback replyArgData)
        (callback replyRejectData)
        (bytes arg)

managementService :: HasEndpoint => IC00 -> Rec (ICManagement IO)
managementService ic00 =
  Candid.toCandidService err $ \method_name arg ->
    ic00 method_name arg >>= callReply
  where
    err s = assertFailure $ "Candid decoding error: " ++ s

ic_create :: HasEndpoint => IC00 -> IO Blob
ic_create ic00 = do
  r <- managementService ic00 .! #create_canister $ ()
  return (rawPrincipal (r .! #canister_id))

ic_install :: HasEndpoint => IC00 -> InstallMode -> Blob -> Blob -> Blob -> IO ()
ic_install ic00 mode canister_id wasm_module arg = do
  managementService ic00 .! #install_code $ empty
    .+ #mode .== mode
    .+ #canister_id .== Principal canister_id
    .+ #wasm_module .== wasm_module
    .+ #arg .== arg
    .+ #compute_allocation .== Nothing

ic_set_controller :: HasEndpoint => IC00 -> Blob -> Blob -> IO ()
ic_set_controller ic00 canister_id new_controller = do
  managementService ic00 .! #set_controller $ empty
    .+ #canister_id .== Principal canister_id
    .+ #new_controller .== Principal new_controller

ic_start_canister :: HasEndpoint => IC00 -> Blob -> IO ()
ic_start_canister ic00 canister_id = do
  managementService ic00 .! #start_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_stop_canister :: HasEndpoint => IC00 -> Blob -> IO ()
ic_stop_canister ic00 canister_id = do
  managementService ic00 .! #stop_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_canister_status :: HasEndpoint => IC00 -> Blob -> IO RunState
ic_canister_status ic00 canister_id = do
  r <- managementService ic00 .! #canister_status $ empty
    .+ #canister_id .== Principal canister_id
  return (r .! #status)

ic_delete_canister :: HasEndpoint => IC00 -> Blob -> IO ()
ic_delete_canister ic00 canister_id = do
  managementService ic00 .! #delete_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_raw_rand :: HasEndpoint => IC00 -> IO Blob
ic_raw_rand ic00 = managementService ic00 .! #raw_rand $ ()

-- Primed variants return the request
callIC' :: forall s a b.
  HasEndpoint =>
  KnownSymbol s =>
  (a -> IO b) ~ (ICManagement IO .! s) =>
  Candid.CandidArg a =>
  IC00 -> Label s -> a -> IO GenR
callIC' ic00 l x = ic00 (T.pack (symbolVal l)) (Candid.encode x)

ic_install' :: HasEndpoint => IC00 -> InstallMode -> Blob -> Blob -> Blob -> IO GenR
ic_install' ic00 mode canister_id wasm_module arg =
  callIC' ic00 #install_code $ empty
    .+ #mode .== mode
    .+ #canister_id .== Principal canister_id
    .+ #wasm_module .== wasm_module
    .+ #arg .== arg
    .+ #compute_allocation .== Nothing

ic_set_controller' :: HasEndpoint => IC00 -> Blob -> Blob -> IO GenR
ic_set_controller' ic00 canister_id new_controller = do
  callIC' ic00 #set_controller $ empty
    .+ #canister_id .== Principal canister_id
    .+ #new_controller .== Principal new_controller

ic_start_canister' :: HasEndpoint => IC00 -> Blob -> IO GenR
ic_start_canister' ic00 canister_id = do
  callIC' ic00 #start_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_stop_canister' :: HasEndpoint => IC00 -> Blob -> IO GenR
ic_stop_canister' ic00 canister_id = do
  callIC' ic00 #stop_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_canister_status' :: HasEndpoint => IC00 -> Blob -> IO GenR
ic_canister_status' ic00 canister_id = do
  callIC' ic00 #canister_status $ empty
    .+ #canister_id .== Principal canister_id

ic_delete_canister' :: HasEndpoint => IC00 -> Blob -> IO GenR
ic_delete_canister' ic00 canister_id = do
  callIC' ic00 #delete_canister $ empty
    .+ #canister_id .== Principal canister_id


-- * Interacting with the universal canister

-- Some common operations on the universal canister
-- The primed variant (call') return the response record,
-- e.g. to check for error conditions.
-- The unprimed variant expect a reply.

install' :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO GenR
install' cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install' ic00 (enum #install) cid universal_wasm (run prog)

-- Also calls create, used default 'ic00'
install :: (HasCallStack, HasEndpoint) => Prog -> IO Blob
install prog = do
    cid <- ic_create ic00
    universal_wasm <- getTestWasm "universal_canister"
    ic_install ic00 (enum #install) cid universal_wasm (run prog)
    return cid

upgrade' :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO GenR
upgrade' cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install' ic00 (enum #upgrade) cid universal_wasm (run prog)

upgrade :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO ()
upgrade cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install ic00 (enum #upgrade) cid universal_wasm (run prog)

reinstall' :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO GenR
reinstall' cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install' ic00 (enum #reinstall) cid universal_wasm (run prog)

reinstall :: (HasCallStack, HasEndpoint) => Blob -> Prog -> IO ()
reinstall cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install ic00 (enum #reinstall) cid universal_wasm (run prog)

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


-- Convenience around Data.Row.Variants used as enums

enum :: (AllUniqueLabels r, KnownSymbol l, (r .! l) ~ ()) => Label l -> Var r
enum l = V.IsJust l ()
