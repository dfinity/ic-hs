{- |

This module contains a test suite for the Internet Computer

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module IC.Test.Spec (icTests) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.Vector as Vec
import Numeric.Natural
import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Codec.Compression.GZip (compress)
import Control.Monad
import Data.Word
import Data.Functor
import Data.Row as R
import Data.Time.Clock.POSIX
import Codec.Candid (Principal(..))
import qualified Codec.Candid as Candid
import Data.Serialize.LEB128 (toLEB128)

import IC.Types (EntityId(..))
import IC.HTTP.GenR
import IC.HTTP.RequestId
import IC.Crypto
import qualified IC.Crypto.CanisterSig as CanisterSig
import qualified IC.Crypto.DER as DER
import IC.Id.Forms hiding (Blob)
import IC.Test.Universal
import IC.HashTree hiding (Blob, Label)
import IC.Certificate
import IC.Hash
import IC.Test.Agent
import IC.Test.Agent.Calls
import IC.Test.Spec.Utils
import qualified IC.Test.Spec.TECDSA

-- * The test suite (see below for helper functions)

icTests :: AgentConfig -> TestTree
icTests = withAgentConfig $ testGroup "Interface Spec acceptance tests"
  [ simpleTestCase "create and install" $ \_ ->
      return ()

  , testCase "create_canister necessary" $
      ic_install'' defaultUser (enum #install) doesn'tExist trivialWasmModule ""
          >>= isErrOrReject [3,5]

  , testCaseSteps "management requests" $ \step -> do
      step "Create (provisional)"
      can_id <- create

      step "Install"
      ic_install ic00 (enum #install) can_id trivialWasmModule ""

      step "Install again fails"
      ic_install'' defaultUser (enum #install) can_id trivialWasmModule ""
        >>= isErrOrReject [3,5]

      step "Reinstall"
      ic_install ic00 (enum #reinstall) can_id trivialWasmModule ""

      step "Reinstall as wrong user"
      ic_install'' otherUser (enum #reinstall) can_id trivialWasmModule ""
        >>= isErrOrReject [3,5]

      step "Upgrade"
      ic_install ic00 (enum #upgrade) can_id trivialWasmModule ""

      step "Upgrade as wrong user"
      ic_install'' otherUser (enum #upgrade) can_id trivialWasmModule ""
        >>= isErrOrReject [3,5]

      step "Change controller"
      ic_set_controllers ic00 can_id [otherUser]

      step "Change controller (with wrong controller)"
      ic_set_controllers'' defaultUser can_id [otherUser]
        >>= isErrOrReject [3,5]

      step "Reinstall as new controller"
      ic_install (ic00as otherUser) (enum #reinstall) can_id trivialWasmModule ""

  , testCaseSteps "install (gzip compressed)" $ \step -> do
      cid <- create
      let compressedModule = compress trivialWasmModule

      step "Install compressed module"
      ic_install ic00 (enum #install) cid compressedModule""

      cs <- ic_canister_status ic00 cid
      cs .! #module_hash @?= Just (sha256 compressedModule)

      step "Reinstall compressed module"
      ic_install ic00 (enum #reinstall) cid compressedModule ""

      cs <- ic_canister_status ic00 cid
      cs .! #module_hash @?= Just (sha256 compressedModule)

      step "Install raw module"
      ic_install ic00 (enum #reinstall) cid trivialWasmModule ""

      cs <- ic_canister_status ic00 cid
      cs .! #module_hash @?= Just (sha256 trivialWasmModule)

      step "Upgrade to a compressed module"
      ic_install ic00 (enum #upgrade) cid compressedModule ""

      cs <- ic_canister_status ic00 cid
      cs .! #module_hash @?= Just (sha256 compressedModule)

  , testCaseSteps "reinstall on empty" $ \step -> do
      step "Create"
      can_id <- create

      step "Reinstall over empty canister"
      ic_install ic00 (enum #reinstall) can_id trivialWasmModule ""

  , testCaseSteps "canister_status" $ \step -> do
      step "Create empty"
      cid <- create
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running
      cs .! #settings .! #controllers @?= Vec.fromList [Principal defaultUser]
      cs .! #module_hash @?= Nothing

      step "Install"
      ic_install ic00 (enum #install) cid trivialWasmModule ""
      cs <- ic_canister_status ic00 cid
      cs .! #module_hash @?= Just (sha256 trivialWasmModule)

  , testCaseSteps "canister lifecycle" $ \step -> do
      cid <- install $
        onPreUpgrade $ callback $
          ignore (stableGrow (int 1)) >>>
          stableWrite (int 0) (i2b getStatus)

      step "Is running (via management)?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running

      step "Is running (local)?"
      query cid (replyData (i2b getStatus)) >>= asWord32 >>= is 1

      step "Stop"
      ic_stop_canister ic00 cid

      step "Is stopped (via management)?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #stopped

      step "Stop is noop"
      ic_stop_canister ic00 cid

      step "Cannot call (update)?"
      call'' cid reply >>= isErrOrReject [5]

      step "Cannot call (query)?"
      query' cid reply >>= isReject [5]

      step "Upgrade"
      upgrade cid $ setGlobal (i2b getStatus)

      step "Start canister"
      ic_start_canister ic00 cid

      step "Is running (via managemnet)?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running

      step "Is running (local)?"
      query cid (replyData (i2b getStatus)) >>= asWord32 >>= is 1

      step "Was stopped during pre-upgrade?"
      query cid (replyData (stableRead (int 0) (int 4))) >>= asWord32 >>= is 3

      step "Was stopped during post-upgrade?"
      query cid (replyData getGlobal) >>= asWord32 >>= is 3

      step "Can call (update)?"
      call_ cid reply

      step "Can call (query)?"
      query_ cid reply

      step "Start is noop"
      ic_start_canister ic00 cid

  , testCaseSteps "canister stopping" $ \step -> do
      cid <- install noop

      step "Is running (via management)?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running

      step "Is running (local)?"
      query cid (replyData (i2b getStatus)) >>= asWord32 >>= is 1

      step "Create message hold"
      (messageHold, release) <- createMessageHold

      step "Create long-running call"
      grs1 <- submitCall cid $ callRequest cid messageHold
      awaitKnown grs1 >>= isPendingOrProcessing

      step "Normal call (to sync)"
      call_ cid reply

      step "Stop"
      grs2 <- submitCall cid $ stopRequest cid
      awaitKnown grs2 >>= isPendingOrProcessing

      step "Is stopping (via management)?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #stopping

      step "Next stop waits, too"
      grs3 <- submitCall cid $ stopRequest cid
      awaitKnown grs3 >>= isPendingOrProcessing

      step "Cannot call (update)?"
      call'' cid reply >>= isErrOrReject [5]

      step "Cannot call (query)?"
      query' cid reply >>= isReject [5]

      step "Release the held message"
      release

      step "Wait for calls to complete"
      awaitStatus grs1 >>= isReply >>= is ""
      awaitStatus grs2 >>= isReply >>= is (Candid.encode ())
      awaitStatus grs3 >>= isReply >>= is (Candid.encode ())

      step "Is stopped (via management)?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #stopped


      step "Cannot call (update)?"
      call'' cid reply >>= isErrOrReject [5]

      step "Cannot call (query)?"
      query' cid reply >>= isReject [5]

  , testCaseSteps "canister deletion" $ \step -> do
      cid <- install noop

      step "Deletion fails"
      ic_delete_canister' ic00 cid >>= isReject [5]

      step "Create message hold"
      (messageHold, release) <- createMessageHold

      step "Create long-running call"
      grs1 <- submitCall cid $ callRequest cid messageHold
      awaitKnown grs1 >>= isPendingOrProcessing

      step "Start stopping"
      grs2 <- submitCall cid $ stopRequest cid
      awaitKnown grs2 >>= isPendingOrProcessing

      step "Is stopping?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #stopping

      step "Deletion fails"
      ic_delete_canister' ic00 cid >>= isReject [5]

      step "Let canister stop"
      release
      awaitStatus grs1 >>= isReply >>= is ""
      awaitStatus grs2 >>= isReply >>= is (Candid.encode ())

      step "Is stopped?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #stopped

      step "Deletion succeeds"
      ic_delete_canister ic00 cid

      -- Disabled; such a call gets accepted (200) but
      -- then the status never shows up, which causes a timeout
      --
      -- step "Cannot call (update)?"
      -- call' cid reply >>= isReject [3]

      step "Cannot call (inter-canister)?"
      cid2 <- install noop
      do call cid2 $ inter_update cid defArgs
        >>= isRelay >>= isReject [3]

      step "Cannot call (query)?"
      query' cid reply >>= isReject [3]

      step "Cannot query canister_status"
      ic_canister_status'' defaultUser cid >>= isErrOrReject [3,5]

      step "Deletion fails"
      ic_delete_canister'' defaultUser cid >>= isErrOrReject [3,5]


  , testCaseSteps "canister lifecycle (wrong controller)" $ \step -> do
      cid <- install noop

      step "Start as wrong user"
      ic_start_canister'' otherUser cid >>= isErrOrReject [3,5]
      step "Stop as wrong user"
      ic_stop_canister'' otherUser cid >>= isErrOrReject [3,5]
      step "Canister Status as wrong user"
      ic_canister_status'' otherUser cid >>= isErrOrReject [3,5]
      step "Delete as wrong user"
      ic_delete_canister'' otherUser cid >>= isErrOrReject [3,5]


  , testCaseSteps "aaaaa-aa (inter-canister)" $ \step -> do
    -- install universal canisters to proxy the requests
    cid <- install noop
    cid2 <- install noop

    step "Create"
    can_id <- ic_provisional_create (ic00via cid) Nothing empty

    step "Install"
    ic_install (ic00via cid) (enum #install) can_id trivialWasmModule ""

    step "Install again fails"
    ic_install' (ic00via cid) (enum #install) can_id trivialWasmModule ""
      >>= isReject [3,5]

    step "Reinstall"
    ic_install (ic00via cid) (enum #reinstall) can_id trivialWasmModule ""

    step "Reinstall (gzip compressed)"
    ic_install (ic00via cid) (enum #reinstall) can_id (compress trivialWasmModule) ""

    step "Reinstall as wrong user"
    ic_install' (ic00via cid2) (enum #reinstall) can_id trivialWasmModule ""
      >>= isReject [3,5]

    step "Upgrade"
    ic_install (ic00via cid) (enum #upgrade) can_id trivialWasmModule ""

    step "Change controller"
    ic_set_controllers (ic00via cid) can_id [cid2]

    step "Change controller (with wrong controller)"
    ic_set_controllers' (ic00via cid) can_id [cid2]
      >>= isReject [3,5]

    step "Reinstall as new controller"
    ic_install (ic00via cid2) (enum #reinstall) can_id trivialWasmModule ""

    step "Create"
    can_id2 <- ic_provisional_create (ic00via cid) Nothing empty

    step "Reinstall on empty"
    ic_install (ic00via cid) (enum #reinstall) can_id2 trivialWasmModule ""

  , simpleTestCase "aaaaa-aa (inter-canister, large)" $ \cid -> do
    universal_wasm <- getTestWasm "universal_canister"
    can_id <- ic_provisional_create (ic00via cid) Nothing empty
    ic_install (ic00via cid) (enum #install) can_id universal_wasm ""
    do call can_id $ replyData "Hi"
      >>= is "Hi"

  , simpleTestCase "randomness" $ \cid -> do
    r1 <- ic_raw_rand (ic00via cid)
    r2 <- ic_raw_rand (ic00via cid)
    BS.length r1 @?= 32
    BS.length r2 @?= 32
    assertBool "random blobs are different" $ r1 /= r2

  , IC.Test.Spec.TECDSA.tests
  , testGroup "canister http calls"
    [ simpleTestCase "simple call, no transform" $ \cid -> do
      resp <- ic_http_request (ic00via cid) cid Nothing
      (resp .! #status) @?= 200
      (resp .! #body) @?= "Hello world!"

    , testCase "non-existent transform function" $ do
      cid <- install noop
      ic_http_request' (ic00via cid) cid (Just "nonExistent", cid) >>= isReject [5]

    , testCase "reference to a transform function exposed by another canister" $ do
      cid <- install noop
      cid2 <- install (onTransform (callback (replyData (bytes (Candid.encode dummyResponse)))))
      ic_http_request' (ic00via cid) cid (Just "transform", cid2) >>= isReject [5]

    , testCase "simple call with transform" $ do
      cid <- install (onTransform (callback (replyData (bytes (Candid.encode dummyResponse)))))
      resp <- ic_http_request (ic00via cid) cid (Just "transform")
      (resp .! #status) @?= 202
      (resp .! #body) @?= "Dummy!"
    ]

  , testGroup "simple calls"
    [ simpleTestCase "Call" $ \cid ->
      call cid (replyData "ABCD") >>= is "ABCD"

    , simpleTestCase "Call (query)" $ \cid -> do
      query cid (replyData "ABCD") >>= is "ABCD"

    , simpleTestCase "Call no non-existant update method" $ \cid ->
      do awaitCall' cid $ rec
          [ "request_type" =: GText "call"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "no_such_update"
          , "arg" =: GBlob ""
          ]
        >>= isErrOrReject [3]

    , simpleTestCase "Call no non-existant query method" $ \cid ->
      do queryCBOR cid $ rec
          [ "request_type" =: GText "query"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "no_such_update"
          , "arg" =: GBlob ""
          ]
        >>= queryResponse >>= isReject [3]

    , simpleTestCase "reject" $ \cid ->
      call' cid (reject "ABCD") >>= isReject [4]

    , simpleTestCase "reject (query)" $ \cid ->
      query' cid (reject "ABCD") >>= isReject [4]

    , simpleTestCase "No response" $ \cid ->
      call' cid noop >>= isReject [5]

    , simpleTestCase "No response does not rollback" $ \cid -> do
      call'' cid (setGlobal "FOO") >>= isErrOrReject [5]
      query cid (replyData getGlobal) >>= is "FOO"

    , simpleTestCase "No response (query)" $ \cid ->
      query' cid noop >>= isReject [5]

    , simpleTestCase "Double reply" $ \cid ->
      call' cid (reply >>> reply) >>= isReject [5]

    , simpleTestCase "Double reply (query)" $ \cid ->
      query' cid (reply >>> reply) >>= isReject [5]

    , simpleTestCase "Reply data append after reply" $ \cid ->
      call' cid (reply >>> replyDataAppend "foo") >>= isReject [5]

    , simpleTestCase "Reply data append after reject" $ \cid ->
      call' cid (reject "bar" >>> replyDataAppend "foo") >>= isReject [5]

    , simpleTestCase "Caller" $ \cid ->
      call cid (replyData caller) >>= is defaultUser

    , simpleTestCase "Caller (query)" $ \cid ->
      query cid (replyData caller) >>= is defaultUser
    ]

  , testGroup "Settings"
    [ testGroup "Controllers"
        [testCase "Multiple controllers (provisional)" $ do
        let controllers = [Principal defaultUser, Principal otherUser]
        cid <- ic_provisional_create ic00 Nothing (#controllers .== Vec.fromList controllers)

        -- Controllers should be able to fetch the canister status.
        cs <- ic_canister_status (ic00as defaultUser) cid
        Vec.toList (cs .! #settings .! #controllers) `isSet` controllers
        cs <- ic_canister_status (ic00as otherUser) cid
        Vec.toList (cs .! #settings .! #controllers) `isSet` controllers

        -- Non-controllers cannot fetch the canister status
        ic_canister_status'' ecdsaUser cid >>= isErrOrReject [3, 5]
        ic_canister_status'' anonymousUser cid >>= isErrOrReject [3, 5]
        ic_canister_status'' secp256k1User cid >>= isErrOrReject [3, 5]

        -- Set new controller
        ic_set_controllers (ic00as defaultUser) cid [ecdsaUser]

        -- Only that controller can get canister status
        ic_canister_status'' defaultUser cid >>= isErrOrReject [3, 5]
        ic_canister_status'' otherUser cid >>= isErrOrReject [3, 5]
        ic_canister_status'' anonymousUser cid >>= isErrOrReject [3, 5]
        ic_canister_status'' secp256k1User cid >>= isErrOrReject [3, 5]
        cs <- ic_canister_status (ic00as ecdsaUser) cid
        cs .! #settings .! #controllers @?= Vec.fromList [Principal ecdsaUser]

    , simpleTestCase "Multiple controllers (aaaaa-aa)" $ \cid -> do
        let controllers = [Principal cid, Principal otherUser]
        cid2 <- ic_create (ic00viaWithCycles cid 20_000_000_000_000) (#controllers .== Vec.fromList controllers)

        -- Controllers should be able to fetch the canister status.
        cs <- ic_canister_status (ic00via cid) cid2
        Vec.toList (cs .! #settings .! #controllers) `isSet` controllers
        cs <- ic_canister_status (ic00as otherUser) cid2
        Vec.toList (cs .! #settings .! #controllers) `isSet` controllers

        -- Set new controller
        ic_set_controllers (ic00via cid) cid2 [ecdsaUser]

        -- Only that controller can get canister status
        ic_canister_status'' defaultUser cid2 >>= isErrOrReject [3, 5]
        ic_canister_status'' otherUser cid2 >>= isErrOrReject [3, 5]
        cs <- ic_canister_status (ic00as ecdsaUser) cid2
        cs .! #settings .! #controllers @?= Vec.fromList [Principal ecdsaUser]

    , simpleTestCase "> 10 controllers" $ \cid -> do
        ic_create' (ic00viaWithCycles cid 20_000_000_000_000) (#controllers .== Vec.fromList (replicate 11 (Principal cid)))
           >>= isReject [3,5]

    , simpleTestCase "No controller" $ \cid -> do
        cid2 <- ic_create (ic00viaWithCycles cid 20_000_000_000_000) (#controllers .== Vec.fromList [])
        ic_canister_status'' defaultUser cid2 >>= isErrOrReject [3, 5]
        ic_canister_status'' otherUser cid2 >>= isErrOrReject [3, 5]

    , testCase "Controller is self" $ do
        cid <- install noop
        ic_set_controllers ic00 cid [cid] -- Set controller of cid to be itself

        -- cid can now request its own status
        cs <- ic_canister_status (ic00via cid) cid
        cs .! #settings .! #controllers @?= Vec.fromList [Principal cid]

    , testCase "Duplicate controllers" $ do
        let controllers = [Principal defaultUser, Principal defaultUser, Principal otherUser]
        cid <- ic_provisional_create ic00 Nothing (#controllers .== Vec.fromList controllers)
        cs <- ic_canister_status (ic00as defaultUser) cid
        Vec.toList (cs .! #settings .! #controllers) `isSet` controllers
    ]

    , simpleTestCase "Valid allocations" $ \cid -> do
        cid2 <- ic_create (ic00viaWithCycles cid 20_000_000_000_000) $ empty
          .+ #compute_allocation .== (1::Natural)
          .+ #memory_allocation .== (1024*1024::Natural)
          .+ #freezing_threshold .== 1000_000
        cs <- ic_canister_status (ic00via cid) cid2
        cs .! #settings .! #compute_allocation @?= 1
        cs .! #settings .! #memory_allocation @?= 1024*1024
        cs .! #settings .! #freezing_threshold @?= 1000_000

    , testGroup "via provisional_create_canister_with_cycles:"
        [ testCase "Invalid compute allocation" $ do
            ic_provisional_create' ic00 Nothing (#compute_allocation .== 101)
               >>= isReject [3,5]
        , testCase "Invalid memory allocation (2^49)" $ do
            ic_provisional_create' ic00 Nothing (#compute_allocation .== 2^(49::Int))
               >>= isReject [3,5]
        , testCase "Invalid memory allocation (2^70)" $ do
            ic_provisional_create' ic00 Nothing (#compute_allocation .== 2^(70::Int))
               >>= isReject [3,5]
        , testCase "Invalid freezing threshold (2^70)" $ do
            ic_provisional_create' ic00 Nothing (#freezing_threshold .== 2^(70::Int))
               >>= isReject [3,5]
        ]
    , testGroup "via create_canister:"
        [ simpleTestCase "Invalid compute allocation" $ \cid -> do
            ic_create' (ic00via cid) (#compute_allocation .== 101)
               >>= isReject [3,5]
        , simpleTestCase "Invalid memory allocation (2^49)" $ \cid -> do
            ic_create' (ic00via cid) (#compute_allocation .== 2^(49::Int))
               >>= isReject [3,5]
        , simpleTestCase "Invalid memory allocation (2^70)" $ \cid -> do
            ic_create' (ic00via cid) (#compute_allocation .== 2^(70::Int))
               >>= isReject [3,5]
        , simpleTestCase "Invalid freezing threshold (2^70)" $ \cid -> do
            ic_create' (ic00via cid) (#freezing_threshold .== 2^(70::Int))
               >>= isReject [3,5]
        ]
    , testGroup "via update_settings"
        [ simpleTestCase "Invalid compute allocation" $ \cid -> do
            ic_update_settings' ic00 cid (#compute_allocation .== 101)
               >>= isReject [3,5]
        , simpleTestCase "Invalid memory allocation (2^49)" $ \cid -> do
            ic_update_settings' ic00 cid (#compute_allocation .== 2^(49::Int))
               >>= isReject [3,5]
        , simpleTestCase "Invalid memory allocation (2^70)" $ \cid -> do
            ic_update_settings' ic00 cid (#compute_allocation .== 2^(70::Int))
               >>= isReject [3,5]
        , simpleTestCase "Invalid freezing threshold (2^70)" $ \cid -> do
            ic_update_settings' ic00 cid (#freezing_threshold .== 2^(70::Int))
               >>= isReject [3,5]
        ]
    ]

  , testGroup "anonymous user"
    [ simpleTestCase "update, sender absent fails" $ \cid ->
      do envelopeFor anonymousUser $ rec
          [ "request_type" =: GText "call"
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "update"
          , "arg" =: GBlob (run (replyData caller))
          ]
        >>= postCallCBOR cid >>= code4xx
    , simpleTestCase "query, sender absent fails" $ \cid ->
      do envelopeFor anonymousUser $ rec
          [ "request_type" =: GText "query"
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "query"
          , "arg" =: GBlob (run (replyData caller))
          ]
        >>= postQueryCBOR cid >>= code4xx
    , simpleTestCase "update, sender explicit" $ \cid ->
      do awaitCall cid $ rec
          [ "request_type" =: GText "call"
          , "canister_id" =: GBlob cid
          , "sender" =: GBlob anonymousUser
          , "method_name" =: GText "update"
          , "arg" =: GBlob (run (replyData caller))
          ]
        >>= isReply >>= is anonymousUser
    , simpleTestCase "query, sender explicit" $ \cid ->
      do queryCBOR cid $ rec
          [ "request_type" =: GText "query"
          , "canister_id" =: GBlob cid
          , "sender" =: GBlob anonymousUser
          , "method_name" =: GText "query"
          , "arg" =: GBlob (run (replyData caller))
          ]
        >>= queryResponse >>= isReply >>= is anonymousUser
    ]

  , testGroup "state"
    [ simpleTestCase "set/get" $ \cid -> do
      call_ cid $ setGlobal "FOO" >>> reply
      query cid (replyData getGlobal) >>= is "FOO"

    , simpleTestCase "set/set/get" $ \cid -> do
      call_ cid $ setGlobal "FOO" >>> reply
      call_ cid $ setGlobal "BAR" >>> reply
      query cid (replyData getGlobal) >>= is "BAR"

    , simpleTestCase "resubmission" $ \cid -> do
      -- Submits the same request (same nonce) twice, checks that
      -- the IC does not act twice.
      -- (Using growing stable memory as non-idempotent action)
      callTwice' cid (ignore (stableGrow (int 1)) >>> reply) >>= isReply >>= is ""
      query cid (replyData (i2b stableSize)) >>= is "\1\0\0\0"
    ]

  , testGroup "API availability" $
    {-
    This section checks various API calls in various contexts, to see
    if they trap when they should
    This mirrors the table in https://sdk.dfinity.org/docs/interface-spec/index.html#system-api-imports

    -}
    let
      {-
      Contexts

      A context is a function of type
         (String, Prog -> TestCase, Prog -> TestCase)
      building a test for does-not-trap or does-trap
      -}
      contexts = mconcat
        [ "I" =: twoContexts
          (reqResponse (\prog -> do
            cid <- create
            install' cid prog
          ))
          (reqResponse (\prog -> do
            cid <- install noop
            upgrade' cid prog
          ))
        , "G" =: reqResponse (\prog -> do
            cid <- install (onPreUpgrade (callback prog))
            upgrade' cid noop
          )
        , "U" =: twoContexts
          (reqResponse (\prog -> do
            cid <- install noop
            call' cid (prog >>> reply)
          ))
          (reqResponse (\prog -> do
            cid <- install noop
            call cid >=> isRelay $ inter_update cid defArgs{
              other_side = prog >>> reply
            }
          ))
        , "Q" =: twoContexts
          (reqResponse (\prog -> do
            cid <- install noop
            query' cid (prog >>> reply)
          ))
          (reqResponse (\prog -> do
            cid <- install noop
            call cid >=> isRelay $ inter_query cid defArgs{
              other_side = prog >>> reply
            }
          ))
        , "Ry" =: reqResponse (\prog -> do
            cid <- install noop
            call' cid $ inter_query cid defArgs{
              on_reply = prog >>> reply
            }
          )
        , "Rt" =: reqResponse (\prog -> do
            cid <- install noop
            call' cid $ inter_query cid defArgs{
              on_reject = prog >>> reply,
              other_side = trap "trap!"
            }
          )
        , "C" =: boolTest (\prog -> do
            cid <- install noop
            call' cid >=> isReject [5] $ inter_query cid defArgs
              { other_side = reply
              , on_reply = trap "Trapping in on_reply"
              , on_cleanup = Just $ prog >>> setGlobal "Did not trap"
              }
            g <- query cid $ replyData getGlobal
            return (g == "Did not trap")
          )
        , "F" =: httpResponse (\prog -> do
            cid <- install (onInspectMessage (callback (prog >>> acceptMessage)))
            call'' cid reply
          )
        , "H" =: boolTest (\prog -> do
            cid <- install (onHeartbeat (callback (prog >>> setGlobal "Did not trap")))
            call_ cid reply -- This assumes that after one update call returned, a heartbeat
                            -- should have happened. Also see heartbeat tests below.
            g <- query cid $ replyData getGlobal
            return (g == "Did not trap")
          )
        ]

      -- context builder helpers
      httpResponse act = (act >=> is2xx >=> void . isReply, act >=> isErrOrReject [5])
      reqResponse act = (act >=> void . isReply, act >=> isReject [5])
      boolTest act = (act >=> is True, act >=> is False)
      twoContexts (aNT1, aT1) (aNT2, aT2) = (\p -> aNT1 p >> aNT2 p,\p -> aT1 p >> aT2 p)

      -- assembling it all
      t name trapping prog
        | Just n <- find (not . (`HM.member` contexts)) s
        = error $ "Undefined context " ++ T.unpack n
        | otherwise =
        [ if cname `S.member` s
          then testCase (name ++ " works in " ++ T.unpack cname) $ actNT prog
          else testCase (name ++ " traps in " ++ T.unpack cname) $ actTrap prog
        | (cname, (actNT, actTrap)) <- HM.toList contexts
        ]
        where s = S.fromList (T.words trapping)

      star = "I G U Q Ry Rt C F H"
      never = ""

    in concat
    [ t "msg_arg_data"                 "I U Q Ry F"  $ ignore argData
    , t "msg_caller"                   "I G U Q F"   $ ignore caller
    , t "msg_reject_code"              "Ry Rt"       $ ignore reject_code
    , t "msg_reject_msg"               "Rt"          $ ignore reject_msg
    , t "msg_reply_data_append"        "U Q Ry Rt"   $ replyDataAppend "Hey!"
    , t "msg_reply_data_append (\"\")" "U Q Ry Rt"   $ replyDataAppend ""
    , t "msg_reply"                    never           reply -- due to double reply
    , t "msg_reject"                   never         $ reject "rejecting" -- due to double reply
    , t "msg_cycles_available"         "U Rt Ry"     $ ignore getAvailableCycles
    , t "msg_cycles_available128"      "U Rt Ry"     $ ignore getAvailableCycles128
    , t "msg_cycles_refunded"          "Rt Ry"       $ ignore getRefund
    , t "msg_cycles_refunded128"       "Rt Ry"       $ ignore getRefund128
    , t "msg_cycles_accept"            "U Rt Ry"     $ ignore (acceptCycles (int64 0))
    , t "msg_cycles_accept128"         "U Rt Ry"     $ ignore (acceptCycles128 (int64 0) (int64 0))
    , t "canister_self"                star          $ ignore self
    , t "canister_cycle_balance"       star          $ ignore getBalance
    , t "canister_cycle_balance128"    star          $ ignore getBalance128
    , t "call_new_call_perform"        "U Rt Ry H"   $
        callNew "foo" "bar" "baz" "quux" >>>
        callDataAppend "foo" >>>
        callCyclesAdd (int64 0) >>>
        callPerform
    , t "call_set_cleanup"             never           $ callOnCleanup (callback noop)
    , t "call_data_append"             never           $ callDataAppend "foo"
    , t "call_cycles_add"              never           $ callCyclesAdd (int64 0)
    , t "call_cycles_add128"           never           $ callCyclesAdd128 (int64 0) (int64 0)
    , t "call_perform"                 never             callPerform
    , t "stable_size"                  star            $ ignore stableSize
    , t "stable_grow"                  star            $ ignore $ stableGrow (int 1)
    , t "stable_write"                 star            $ stableWrite (int 0) ""
    , t "stable_read"                  star            $ ignore $ stableRead (int 0) (int 0)
    , t "stable64_size"                star            $ ignore stable64Size
    , t "stable64_grow"                star            $ ignore $ stable64Grow (int64 1)
    , t "stable64_write"               star            $ stable64Write (int64 0) ""
    , t "stable64_read"                star            $ ignore $ stable64Read (int64 0) (int64 0)
    , t "certified_data_set"           "I G U Ry Rt H" $ setCertifiedData "foo"
    , t "data_certificate_present"     star            $ ignore getCertificatePresent
    , t "msg_method_name"              "F"             $ ignore methodName
    , t "accept_message"               never             acceptMessage -- due to double accept
    , t "time"                         star            $ ignore getTime
    , t "performance_counter"          star            $ ignore $ performanceCounter (int 0)
    , t "debug_print"                  star            $ debugPrint "hello"
    , t "trap"                         never           $ trap "this better traps"
    ]

  , simpleTestCase "self" $ \cid ->
    query cid (replyData self) >>= is cid

  , testGroup "wrong url path"
    [ simpleTestCase "call request to query" $ \cid -> do
      let req = rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "update"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postQueryCBOR cid >>= code4xx

    , simpleTestCase "query request to call" $ \cid -> do
      let req = rec
            [ "request_type" =: GText "query"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "query"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postCallCBOR cid >>= code4xx

    , simpleTestCase "query request to read_state" $ \cid -> do
      let req = rec
            [ "request_type" =: GText "query"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "query"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postReadStateCBOR cid >>= code4xx

    , simpleTestCase "read_state request to query" $ \cid -> do
      addNonceExpiryEnv readStateEmpty >>= postQueryCBOR cid >>= code4xx
    ]

  , testGroup "wrong effective canister id"
    [ simpleTestCase "in call" $ \cid1 -> do
      cid2 <- create
      let req = rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid1
            , "method_name" =: GText "update"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postCallCBOR cid2 >>= code4xx

    , simpleTestCase "in query" $ \cid1 -> do
      cid2 <- create
      let req = rec
            [ "request_type" =: GText "query"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid1
            , "method_name" =: GText "query"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postQueryCBOR cid2 >>= code4xx

    -- read_state tested in read_state group
    --
    , simpleTestCase "in mangement call" $ \cid1 -> do
      cid2 <- create
      let req = rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob ""
            , "method_name" =: GText "canister_status"
            , "arg" =: GBlob (Candid.encode (#canister_id .== Principal cid1))
            ]
      addNonceExpiryEnv req >>= postCallCBOR cid2 >>= code4xx

    , simpleTestCase "non-existing (and likely invalid)" $ \cid1 -> do
      let req = rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid1
            , "method_name" =: GText "update"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postCallCBOR "foobar" >>= code4xx

    , simpleTestCase "invalid textual represenation" $ \cid1 -> do
      let req = rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid1
            , "method_name" =: GText "update"
            , "arg" =: GBlob (run reply)
            ]
      let path = "/api/v2/canister/" ++ filter (/= '-') (textual cid1) ++ "/call"
      addNonceExpiryEnv req >>= postCBOR path >>= code4xx
    ]

  , testGroup "inter-canister calls"
    [ testGroup "builder interface"
      [ testGroup "traps without call_new"
        [ simpleTestCase "call_data_append" $ \cid ->
          call' cid (callDataAppend "Foo" >>> reply) >>= isReject [5]
        , simpleTestCase "call_on_cleanup" $ \cid ->
          call' cid (callOnCleanup (callback noop) >>> reply) >>= isReject [5]
        , simpleTestCase "call_cycles_add" $ \cid ->
          call' cid (callCyclesAdd (int64 0) >>> reply) >>= isReject [5]
        , simpleTestCase "call_perform" $ \cid ->
          call' cid (callPerform >>> reply) >>= isReject [5]
        ]
      , simpleTestCase "call_new clears pending call" $ \cid -> do
        do call cid $
            callNew "foo" "bar" "baz" "quux" >>>
            callDataAppend "hey" >>>
            inter_query cid defArgs
          >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid)
      , simpleTestCase "call_data_append really appends" $ \cid -> do
        do call cid $
            callNew (bytes cid) (bytes "query")
                    (callback relayReply) (callback relayReject) >>>
            callDataAppend (bytes (BS.take 3 (run defaultOtherSide))) >>>
            callDataAppend (bytes (BS.drop 3 (run defaultOtherSide))) >>>
            callPerform
         >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid)
      , simpleTestCase "call_on_cleanup traps if called twice" $ \cid ->
        do call' cid $
            callNew (bytes cid) (bytes "query")
                    (callback relayReply) (callback relayReject) >>>
            callOnCleanup (callback noop) >>>
            callOnCleanup (callback noop) >>>
            reply
         >>= isReject [5]
      ]

    , simpleTestCase "to nonexistant canister" $ \cid ->
      call cid (inter_call "foo" "bar" defArgs) >>= isRelay >>= isReject [3]

    , simpleTestCase "to nonexistant canister (user id)" $ \cid ->
      call cid (inter_call defaultUser "bar" defArgs) >>= isRelay >>= isReject [3]

    , simpleTestCase "to nonexistant method" $ \cid ->
      call cid (inter_call cid "bar" defArgs) >>= isRelay >>= isReject [3]

    , simpleTestCase "Call from query method traps (in update call)" $ \cid ->
      callToQuery'' cid (inter_query cid defArgs) >>= is2xx >>= isReject [5]

    , simpleTestCase "Call from query method traps (in query call)" $ \cid ->
      query' cid (inter_query cid defArgs) >>= isReject [5]

    , simpleTestCase "Call from query method traps (in inter-canister-call)" $ \cid ->
      do call cid $
          inter_call cid "query" defArgs {
            other_side = inter_query cid defArgs
          }
        >>= isRelay >>= isReject [5]

    , simpleTestCase "Self-call (to update)" $ \cid ->
      call cid (inter_update cid defArgs)
        >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid)

    , simpleTestCase "Self-call (to query)" $ \cid -> do
      call cid (inter_query cid defArgs)
        >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid)

    , simpleTestCase "update commits" $ \cid -> do
      do call cid $
          setGlobal "FOO" >>>
          inter_update cid defArgs{ other_side = setGlobal "BAR" >>> reply }
       >>= isRelay >>= isReply >>= is ""

      query cid (replyData getGlobal) >>= is "BAR"

    , simpleTestCase "query does not commit" $ \cid -> do
      do call cid $
          setGlobal "FOO" >>>
          inter_query cid defArgs{ other_side = setGlobal "BAR" >>> reply }
       >>= isRelay >>= isReply >>= is ""

      do query cid $ replyData getGlobal
        >>= is "FOO"

    , simpleTestCase "query no response" $ \cid ->
      do call cid $ inter_query cid defArgs{ other_side = noop }
        >>= isRelay >>= isReject [5]

    , simpleTestCase "query double reply" $ \cid ->
      do call cid $ inter_query cid defArgs{ other_side = reply >>> reply }
        >>= isRelay >>= isReject [5]

    , simpleTestCase "Reject code is 0 in reply" $ \cid ->
      do call cid $ inter_query cid defArgs{ on_reply = replyData (i2b reject_code) }
        >>= asWord32 >>= is 0

    , simpleTestCase "Second reply in callback" $ \cid -> do
      do call cid $
          setGlobal "FOO" >>>
          replyData "First reply" >>>
          inter_query cid defArgs{
            on_reply = setGlobal "BAR" >>> replyData "Second reply",
            on_reject = setGlobal "BAZ" >>> relayReject
          }
        >>= is "First reply"

      -- now check that the callback trapped and did not actual change the global
      -- to make this test reliabe, stop and start the canister, this will
      -- ensure all outstanding callbacks are handled
      barrier [cid]

      query cid (replyData getGlobal) >>= is "FOO"

    , simpleTestCase "partial reply" $ \cid ->
      do call cid $
          replyDataAppend "FOO" >>>
          inter_query cid defArgs{ on_reply = replyDataAppend "BAR" >>> reply }
        >>= is "BAR" -- check that the FOO is silently dropped

    , simpleTestCase "cleanup not executed when reply callback does not trap" $ \cid -> do
      call_ cid $ inter_query cid defArgs
        { on_reply = reply
        , on_cleanup = Just (setGlobal "BAD")
        }
      query cid (replyData getGlobal) >>= is ""

    , simpleTestCase "cleanup not executed when reject callback does not trap" $ \cid -> do
      call_ cid $ inter_query cid defArgs
        { other_side = reject "meh"
        , on_reject = reply
        , on_cleanup = Just (setGlobal "BAD")
        }
      query cid (replyData getGlobal) >>= is ""

    , testGroup "two callbacks"
      [ simpleTestCase "reply after trap" $ \cid ->
        do call cid $
            inter_query cid defArgs{ on_reply = trap "first callback traps" } >>>
            inter_query cid defArgs{ on_reply = replyData "good" }
          >>= is "good"

      , simpleTestCase "trap after reply" $ \cid ->
        do call cid $
            inter_query cid defArgs{ on_reply = replyData "good" } >>>
            inter_query cid defArgs{ on_reply = trap "second callback traps" }
         >>= is "good"

      , simpleTestCase "both trap" $ \cid ->
        do call' cid $
            inter_query cid defArgs{ on_reply = trap "first callback traps" } >>>
            inter_query cid defArgs{ on_reply = trap "second callback traps" }
          >>= isReject [5]
      ]

    , simpleTestCase "Call to other canister (to update)" $ \cid -> do
      cid2 <- install noop
      do call cid $ inter_update cid2 defArgs
        >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid2)

    , simpleTestCase "Call to other canister (to query)" $ \cid -> do
      cid2 <- install noop
      do call cid $ inter_query cid2 defArgs
        >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid2)
    ]

  , testCaseSteps "stable memory" $ \step -> do
    cid <- install noop

    step "Stable mem size is zero"
    query cid (replyData (i2b stableSize)) >>= is "\x0\x0\x0\x0"

    step "Writing stable memory (failing)"
    call' cid (stableWrite (int 0) "FOO") >>= isReject [5]
    step "Set stable mem (failing, query)"
    query' cid (stableWrite (int 0) "FOO") >>= isReject [5]

    step "Growing stable memory"
    call cid (replyData (i2b (stableGrow (int 1)))) >>= is "\x0\x0\x0\x0"

    step "Growing stable memory again"
    call cid (replyData (i2b (stableGrow (int 1)))) >>= is "\x1\x0\x0\x0"

    step "Growing stable memory in query"
    query cid (replyData (i2b (stableGrow (int 1)))) >>= is "\x2\x0\x0\x0"

    step "Stable mem size is two"
    query cid (replyData (i2b stableSize)) >>= is "\x2\x0\x0\x0"

    step "Try growing stable memory beyond 4GiB"
    call cid (replyData (i2b (stableGrow (int 65535)))) >>= is "\xff\xff\xff\xff"

    step "Writing stable memory"
    call_ cid $ stableWrite (int 0) "FOO" >>> reply

    step "Writing stable memory (query)"
    query_ cid $ stableWrite (int 0) "BAR" >>> reply

    step "Reading stable memory"
    call cid (replyData (stableRead (int 0) (int 3))) >>= is "FOO"

  , testCaseSteps "64 bit stable memory" $ \step -> do
    cid <- install noop

    step "Stable mem size is zero"
    query cid (replyData (i64tob stable64Size)) >>= is "\x0\x0\x0\x0\x0\x0\x0\x0"

    step "Writing stable memory (failing)"
    call' cid (stable64Write (int64 0) "FOO") >>= isReject [5]

    step "Set stable mem (failing, query)"
    query' cid (stable64Write (int64 0) "FOO") >>= isReject [5]

    step "Growing stable memory"
    call cid (replyData (i64tob (stable64Grow (int64 1)))) >>= is "\x0\x0\x0\x0\x0\x0\x0\x0"

    step "Growing stable memory again"
    call cid (replyData (i64tob (stable64Grow (int64 1)))) >>= is "\x1\x0\x0\x0\x0\x0\x0\x0"

    step "Growing stable memory in query"
    query cid (replyData (i64tob (stable64Grow (int64 1)))) >>= is "\x2\x0\x0\x0\x0\x0\x0\x0"

    step "Stable mem size is two"
    query cid (replyData (i2b stableSize)) >>= is "\x2\x0\x0\x0"
    query cid (replyData (i64tob stable64Size)) >>= is "\x2\x0\x0\x0\x0\x0\x0\x0"

    step "Writing stable memory"
    call_ cid $ stable64Write (int64 0) "FOO" >>> reply

    step "Writing stable memory (query)"
    query_ cid $ stable64Write (int64 0) "BAR" >>> reply

    step "Reading stable memory"
    call cid (replyData (stable64Read (int64 0) (int64 3))) >>= is "FOO"
    call cid (replyData (stableRead (int 0) (int 3))) >>= is "FOO"

    step "Writing in 32 bit mode"
    call_ cid $ stableWrite (int 0) "BAR" >>> reply

    step "Reading back in 64 bit mode"
    call cid (replyData (stable64Read (int64 0) (int64 3))) >>= is "BAR"

    step "Growing stable memory beyond 4GiB"
    call cid (replyData (i64tob (stable64Grow (int64 65535)))) >>= is "\x2\x0\x0\x0\x0\x0\x0\x0"
    query cid (replyData (i64tob stable64Size)) >>= is "\x01\x00\x01\x00\x0\x0\x0\x0"

    step "Using 32 bit API with large stable memory"
    query' cid (ignore stableSize) >>= isReject [5]
    query' cid (ignore $ stableGrow (int 1)) >>= isReject [5]
    query' cid (stableWrite (int 0) "BAZ") >>= isReject [5]
    query' cid (ignore $ stableRead (int 0) (int 3)) >>= isReject [5]

    step "Using 64 bit API with large stable memory"
    call cid (replyData (i64tob (stable64Grow (int64 1)))) >>= is "\x01\x00\x01\x00\x0\x0\x0\x0"
    query cid (replyData (i64tob stable64Size)) >>= is "\x02\x00\x01\x00\x0\x0\x0\x0"
    call cid (replyData (stable64Read (int64 0) (int64 3))) >>= is "BAR"
    call_ cid $ stable64Write (int64 0) "BAZ" >>> reply
    call cid (replyData (stable64Read (int64 0) (int64 3))) >>= is "BAZ"

  , testGroup "time" $
    let getTimeTwice = cat (i64tob getTime) (i64tob getTime) in
    [ simpleTestCase "in query" $ \cid ->
      query cid (replyData getTimeTwice) >>= as2Word64 >>= bothSame
    , simpleTestCase "in update" $ \cid ->
      query cid (replyData getTimeTwice) >>= as2Word64 >>= bothSame
    , testCase "in install" $ do
      cid <- install $ setGlobal getTimeTwice
      query cid (replyData getGlobal) >>= as2Word64 >>= bothSame
    , testCase "in pre_upgrade" $ do
      cid <- install $
        ignore (stableGrow (int 1)) >>>
        onPreUpgrade (callback $ stableWrite (int 0) getTimeTwice)
      upgrade cid noop
      query cid (replyData (stableRead (int 0) (int (2*8)))) >>= as2Word64 >>= bothSame
    , simpleTestCase "in post_upgrade" $ \cid -> do
      upgrade cid $ setGlobal getTimeTwice
      query cid (replyData getGlobal) >>= as2Word64 >>= bothSame
    ]

  , testGroup "upgrades" $
    let installForUpgrade on_pre_upgrade = install $
            setGlobal "FOO" >>>
            ignore (stableGrow (int 1)) >>>
            stableWrite (int 0) "BAR______" >>>
            onPreUpgrade (callback on_pre_upgrade)

        checkNoUpgrade cid = do
          query cid (replyData getGlobal) >>= is "FOO"
          query cid (replyData (stableRead (int 0) (int 9))) >>= is "BAR______"
    in
    [ testCase "succeeding" $ do
      -- check that the pre-upgrade hook has access to the old state
      cid <- installForUpgrade $ stableWrite (int 3) getGlobal
      checkNoUpgrade cid

      upgrade cid $ stableWrite (int 6) (stableRead (int 0) (int 3))

      query cid (replyData getGlobal) >>= is ""
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "BARFOOBAR"

    , testCase "trapping in pre-upgrade" $ do
      cid <- installForUpgrade $ trap "trap in pre-upgrade"
      checkNoUpgrade cid

      upgrade' cid noop >>= isReject [5]
      checkNoUpgrade cid

    , testCase "trapping in pre-upgrade (by calling)" $ do
      cid <- installForUpgrade $ trap "trap in pre-upgrade"
      call_ cid $
        reply >>>
        onPreUpgrade (callback (
            inter_query cid defArgs { other_side = noop }
        ))
      checkNoUpgrade cid

      upgrade' cid noop >>= isReject [5]
      checkNoUpgrade cid

    , testCase "trapping in pre-upgrade (by accessing arg)" $ do
      cid <- installForUpgrade $ ignore argData
      checkNoUpgrade cid

      upgrade' cid noop >>= isReject [5]
      checkNoUpgrade cid

    , testCase "trapping in post-upgrade" $ do
      cid <- installForUpgrade $ stableWrite (int 3) getGlobal
      checkNoUpgrade cid

      upgrade' cid (trap "trap in post-upgrade") >>= isReject [5]
      checkNoUpgrade cid

    , testCase "trapping in post-upgrade (by calling)" $ do
      cid <- installForUpgrade $ stableWrite (int 3) getGlobal
      checkNoUpgrade cid

      do upgrade' cid $ inter_query cid defArgs{ other_side = noop }
        >>= isReject [5]
      checkNoUpgrade cid
    ]

  , testGroup "heartbeat"
    [ testCase "called once for all canisters" $ do
      cid <- install $ onHeartbeat $ callback $ ignore (stableGrow (int 1)) >>> stableWrite (int 0) "FOO"
      cid2 <- install $ onHeartbeat $ callback $ ignore (stableGrow (int 1)) >>> stableWrite (int 0) "BAR"
      -- Heartbeat cannot respond. Should be trapped.
      cid3 <- install $ onHeartbeat $ callback $ setGlobal "FIZZ" >>> replyData "FIZZ"

      -- The spec currently gives no guarantee about when or how frequent heartbeats are executed.
      -- But all implementations have the property: if update call B is submitted after call A is completed,
      -- then a heartbeat runs before the execution of B.
      -- We use this here to make sure that heartbeats have been attempted:
      call_ cid reply
      call_ cid reply

      query cid (replyData (stableRead (int 0) (int 3))) >>= is "FOO"
      query cid2 (replyData (stableRead (int 0) (int 3))) >>= is "BAR"
      query cid3 (replyData getGlobal) >>= is ""
    ]

  , testGroup "reinstall"
    [ testCase "succeeding" $ do
      cid <- install $
            setGlobal "FOO" >>>
            ignore (stableGrow (int 1)) >>>
            stableWrite (int 0) "FOO______"
      query cid (replyData getGlobal) >>= is "FOO"
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "FOO______"
      query cid (replyData (i2b stableSize)) >>= asWord32 >>= is 1

      reinstall cid $
        setGlobal "BAR" >>>
        ignore (stableGrow (int 2)) >>>
        stableWrite (int 0) "BAR______"

      query cid (replyData getGlobal) >>= is "BAR"
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "BAR______"
      query cid (replyData (i2b stableSize)) >>= asWord32 >>= is 2

      reinstall cid noop

      query cid (replyData getGlobal) >>= is ""
      query cid (replyData (i2b stableSize)) >>= asWord32 >>= is 0

    , testCase "trapping" $ do
      cid <- install $
            setGlobal "FOO" >>>
            ignore (stableGrow (int 1)) >>>
            stableWrite (int 0) "FOO______"
      query cid (replyData getGlobal) >>= is "FOO"
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "FOO______"
      query cid (replyData (i2b stableSize)) >>= is "\1\0\0\0"

      reinstall' cid (trap "Trapping the reinstallation") >>= isReject [5]

      query cid (replyData getGlobal) >>= is "FOO"
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "FOO______"
      query cid (replyData (i2b stableSize)) >>= is "\1\0\0\0"
    ]

  , testGroup "uninstall"
    [ testCase "uninstall empty canister" $ do
      cid <- create
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running
      cs .! #settings .! #controllers @?= Vec.fromList [Principal defaultUser]
      cs .! #module_hash @?= Nothing
      ic_uninstall ic00 cid
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running
      cs .! #settings .! #controllers @?= Vec.fromList [Principal defaultUser]
      cs .! #module_hash @?= Nothing

    , testCase "uninstall as wrong user" $ do
      cid <- create
      ic_uninstall'' otherUser cid >>= isErrOrReject [3,5]

    , testCase "uninstall and reinstall wipes state" $ do
      cid <- install (setGlobal "FOO")
      ic_uninstall ic00 cid
      universal_wasm <- getTestWasm "universal_canister"
      ic_install ic00 (enum #install) cid universal_wasm (run (setGlobal "BAR"))
      query cid (replyData getGlobal) >>= is "BAR"

    , testCase "uninstall and reinstall wipes stable memory" $ do
      cid <- install (ignore (stableGrow (int 1)) >>> stableWrite (int 0) "FOO")
      ic_uninstall ic00 cid
      universal_wasm <- getTestWasm "universal_canister"
      ic_install ic00 (enum #install) cid universal_wasm (run (setGlobal "BAR"))
      query cid (replyData (i2b stableSize)) >>= asWord32 >>= is 0
      do query cid $
          ignore (stableGrow (int 1)) >>>
          replyData (stableRead (int 0) (int 3))
       >>= is "\0\0\0"
      do call cid $
          ignore (stableGrow (int 1)) >>>
          replyData (stableRead (int 0) (int 3))
       >>= is "\0\0\0"

    , testCase "uninstall and reinstall wipes certified data" $ do
      cid <- install $ setCertifiedData "FOO"
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
      ic_uninstall ic00 cid
      universal_wasm <- getTestWasm "universal_canister"
      ic_install ic00 (enum #install) cid universal_wasm (run noop)
      query cid (replyData getCertificate) >>= extractCertData cid >>= is ""

    , simpleTestCase "uninstalled rejects calls" $ \cid -> do
      call cid (replyData "Hi") >>= is "Hi"
      query cid (replyData "Hi") >>= is "Hi"
      ic_uninstall ic00 cid
      -- should be http error, due to inspection
      call'' cid (replyData "Hi") >>= isErrOrReject []
      query' cid (replyData "Hi") >>= isReject [3]

    , testCaseSteps "open call contexts are rejected" $ \step -> do
      cid <- install noop

      step "Create message hold"
      (messageHold, release) <- createMessageHold

      step "Create long-running call"
      grs1 <- submitCall cid $ callRequest cid messageHold
      awaitKnown grs1 >>= isPendingOrProcessing

      step "Uninstall"
      ic_uninstall ic00 cid

      step "Long-running call is rejected"
      awaitStatus grs1 >>= isReject [4]

      step "Now release"
      release
      awaitStatus grs1 >>= isReject [4] -- still a reject

    , testCaseSteps "deleted call contexts do not prevent stopping" $ \step -> do
      -- Similar to above, but after uninstalling, imediatelly
      -- stops and deletes. This can only work if the call
      -- contexts are indeed deleted
      cid <- install noop

      step "Create message hold"
      (messageHold, release) <- createMessageHold

      step "Create long-running call"
      grs1 <- submitCall cid $ callRequest cid messageHold
      awaitKnown grs1 >>= isPendingOrProcessing

      step "Uninstall"
      ic_uninstall ic00 cid

      step "Long-running call is rejected"
      awaitStatus grs1 >>= isReject [4]

      step "Stop now"
      ic_stop_canister ic00 cid

      step "Delete now"
      ic_delete_canister ic00 cid

      -- deletion means query fails
      query' cid reply >>= isReject [3]

      step "Now release"
      release
      awaitStatus grs1 >>= isReject [4] -- still a reject
      query' cid reply >>= isReject [3] -- still deleted



    , testCaseSteps "deleted call contexts are not delivered" $ \step -> do
      -- This is a tricky one: We make one long-running call,
      -- then uninstall (rejecting the call), then re-install fresh code,
      -- make another long-running call, then release the first one. The system
      -- should not confuse the two callbacks.
      cid <- install noop
      helper <- install noop

      step "Create message holds"
      (messageHold1, release1) <- createMessageHold
      (messageHold2, release2) <- createMessageHold

      step "Create first long-running call"
      grs1 <- submitCall cid $ callRequest cid $
                inter_call helper "update" defArgs
                  { other_side = messageHold1
                  , on_reply = replyData "First"
                  }
      awaitKnown grs1 >>= isPendingOrProcessing

      step "Uninstall"
      ic_uninstall ic00 cid
      awaitStatus grs1 >>= isReject [4]

      step "Reinstall"
      universal_wasm <- getTestWasm "universal_canister"
      ic_install ic00 (enum #install) cid universal_wasm (run (setGlobal "BAR"))

      step "Create second long-running call"
      grs2 <- submitCall cid $ callRequest cid $
                inter_call helper "update" defArgs
                  { other_side = messageHold2
                  , on_reply = replyData "Second"
                  }
      awaitStatus grs1 >>= isReject [4]
      awaitKnown grs2 >>= isPendingOrProcessing

      step "Release first call"
      release1
      awaitStatus grs1 >>= isReject [4]
      awaitKnown grs2 >>= isPendingOrProcessing

      step "Release second call"
      release2
      awaitStatus grs1 >>= isReject [4]
      awaitStatus grs2 >>= isReply >>= is "Second"
    ]

  , testGroup "debug facilities"
    [ simpleTestCase "Using debug_print" $ \cid ->
      call_ cid (debugPrint "ic-ref-test print" >>> reply)
    , simpleTestCase "Using debug_print (query)" $ \cid ->
      query_ cid $ debugPrint "ic-ref-test print" >>> reply
    , simpleTestCase "Using debug_print with invalid bounds" $ \cid ->
      query_ cid $ badPrint >>> reply
    , simpleTestCase "Explicit trap" $ \cid ->
      call' cid (trap "trapping") >>= isReject [5]
    , simpleTestCase "Explicit trap (query)" $ \cid -> do
      query' cid (trap "trapping") >>= isReject [5]
    ]

  , testCase "caller (in init)" $ do
    cid <- install $ setGlobal caller
    query cid (replyData getGlobal) >>= is defaultUser

  , testCase "self (in init)" $ do
    cid <- install $ setGlobal self
    query cid (replyData getGlobal) >>= is cid

  , testGroup "trapping in init" $
    let
      failInInit pgm = do
        cid <- create
        install' cid pgm >>= isReject [5]
        -- canister does not exist
        query' cid noop >>= isReject [3]
    in
    [ testCase "explicit trap" $ failInInit $ trap "trapping in install"
    , testCase "call" $ failInInit $ inter_query "dummy" defArgs
    , testCase "reply" $ failInInit reply
    , testCase "reject" $ failInInit $ reject "rejecting in init"
    ]

  , testGroup "query"
    [ testGroup "required fields" $ do
        -- TODO: Begin with a succeeding request to a real canister, to rule
        -- out other causes of failure than missing fields
        omitFields queryToNonExistant $ \req -> do
          cid <- create
          addExpiry req >>= envelope defaultSK >>= postQueryCBOR cid >>= code4xx

    , simpleTestCase "non-existing (deleted) canister" $ \cid -> do
        ic_stop_canister ic00 cid
        ic_delete_canister ic00 cid
        query' cid reply >>= isReject [3]

    , simpleTestCase "does not commit" $ \cid -> do
        call_ cid (setGlobal "FOO" >>> reply)
        query cid (setGlobal "BAR" >>> replyData getGlobal) >>= is "BAR"
        query cid (replyData getGlobal) >>= is "FOO"
    ]

  , testGroup "read state" $
    let ensure_request_exists cid user = do
          req <- addNonce >=> addExpiry $ rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob user
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "query"
            , "arg" =: GBlob (run (replyData "\xff\xff"))
            ]
          awaitCall cid req >>= isReply >>= is "\xff\xff"

          -- check that the request is there
          getRequestStatus user cid (requestId req) >>= is (Responded (Reply "\xff\xff"))

          return (requestId req)
        canister_id_in_canister_ranges cid (Just del) = do
            del_cert <- decodeCert' (del_certificate del)
            ranges <- certValue @Blob del_cert ["subnet", del_subnet_id del, "canister_ranges"] >>= asCBORBlobPairList
            cid `isContainedIn` ranges
            canister_id_in_canister_ranges cid (cert_delegation del_cert)
        canister_id_in_canister_ranges _ Nothing = return ()
    in
    [ testGroup "required fields" $
        omitFields readStateEmpty $ \req -> do
          cid <- create
          addExpiry req >>= envelope defaultSK >>= postReadStateCBOR cid >>= code4xx

    , simpleTestCase "certificate validates" $ \cid -> do
        cert <- getStateCert defaultUser cid []
        validateStateCert cert

    , testCaseSteps "time is present" $ \step -> do
        cid <- create
        cert <- getStateCert defaultUser cid []
        time <- certValue @Natural cert ["time"]
        step $ "Reported time: " ++ show time

    , testCase "time can be asked for" $ do
        cid <- create
        cert <- getStateCert defaultUser cid [["time"]]
        void $ certValue @Natural cert ["time"]

    , testCase "controller of empty canister" $ do
        cid <- create
        cert <- getStateCert defaultUser cid [["canister", cid, "controllers"]]
        certValue @Blob cert ["canister", cid, "controllers"] >>= asCBORBlobList >>= isSet [defaultUser]

    , testCase "canister_id included in canister_ranges" $ do
        cid <- create
        cert <- getStateCert defaultUser cid [["subnet"]]
        canister_id_in_canister_ranges cid (cert_delegation cert)

    , testCase "module_hash of empty canister" $ do
        cid <- create
        cert <- getStateCert defaultUser cid [["canister", cid, "module_hash"]]
        lookupPath (cert_tree cert) ["canister", cid, "module_hash"] @?= Absent

    , testCase "single controller of installed canister" $ do
        cid <- install noop
        -- also vary user, just for good measure
        cert <- getStateCert anonymousUser cid [["canister", cid, "controllers"]]
        certValue @Blob cert ["canister", cid, "controllers"] >>= asCBORBlobList >>= isSet [defaultUser]

    , testCase "multiple controllers of installed canister" $ do
        cid <- ic_provisional_create ic00 Nothing (#controllers .== Vec.fromList [Principal defaultUser, Principal otherUser])
        cert <- getStateCert defaultUser cid [["canister", cid, "controllers"]]
        certValue @Blob cert ["canister", cid, "controllers"] >>= asCBORBlobList >>= isSet [defaultUser, otherUser]

    , testCase "zero controllers of installed canister" $ do
        cid <- ic_provisional_create ic00 Nothing (#controllers .== Vec.fromList [])
        cert <- getStateCert defaultUser cid [["canister", cid, "controllers"]]
        certValue @Blob cert ["canister", cid, "controllers"] >>= asCBORBlobList >>= isSet []

    , testCase "module_hash of empty canister" $ do
        cid <- install noop
        universal_wasm <- getTestWasm "universal_canister"
        cert <- getStateCert anonymousUser cid [["canister", cid, "module_hash"]]
        certValue @Blob cert ["canister", cid, "module_hash"] >>= is (sha256 universal_wasm)

    , testGroup "malformed request id"
        [ simpleTestCase ("rid \"" ++ shorten 8 (asHex rid) ++ "\"") $ \cid -> do
            getStateCert' defaultUser cid [["request_status", rid]] >>= code4xx
        | rid <- [ "", "foo" ]
        ]

    , testGroup "non-existence proofs for non-existing request id"
        [ simpleTestCase ("rid \"" ++ shorten 8 (asHex rid) ++ "\"") $ \cid -> do
            cert <- getStateCert defaultUser cid [["request_status", rid]]
            certValueAbsent cert ["request_status", rid, "status"]
        | rid <- [ BS.replicate 32 0, BS.replicate 32 8, BS.replicate 32 255 ]
        ]

    , simpleTestCase "can ask for portion of request status " $ \cid -> do
        rid <- ensure_request_exists cid defaultUser
        cert <- getStateCert defaultUser cid
          [["request_status", rid, "status"], ["request_status", rid, "reply"]]
        void $ certValue @T.Text cert ["request_status", rid, "status"]
        void $ certValue @Blob cert ["request_status", rid, "reply"]

    , simpleTestCase "access denied for other users request" $ \cid -> do
        rid <- ensure_request_exists cid defaultUser
        getStateCert' otherUser cid [["request_status", rid]] >>= code4xx

    , simpleTestCase "reading two statuses to same canister in one go" $ \cid -> do
        rid1 <- ensure_request_exists cid defaultUser
        rid2 <- ensure_request_exists cid defaultUser
        cert <- getStateCert defaultUser cid [["request_status", rid1], ["request_status", rid2]]
        void $ certValue @T.Text cert ["request_status", rid1, "status"]
        void $ certValue @T.Text cert ["request_status", rid2, "status"]

    , simpleTestCase "access denied for other users request (mixed request)" $ \cid -> do
        rid1 <- ensure_request_exists cid defaultUser
        rid2 <- ensure_request_exists cid otherUser
        getStateCert' defaultUser cid [["request_status", rid1], ["request_status", rid2]] >>= code4xx

    , simpleTestCase "access denied two status to different canisters" $ \cid -> do
        cid2 <- install noop
        rid1 <- ensure_request_exists cid defaultUser
        rid2 <- ensure_request_exists cid2 defaultUser
        getStateCert' defaultUser cid [["request_status", rid1], ["request_status", rid2]] >>= code4xx

    , simpleTestCase "access denied for bogus path" $ \cid -> do
        getStateCert' otherUser cid [["hello", "world"]] >>= code4xx

    , simpleTestCase "access denied for fetching full state tree" $ \cid -> do
        getStateCert' otherUser cid [[]] >>= code4xx

    , testGroup "metadata" $
      let withCustomSection mod (name, content) = mod <> BS.singleton 0 <> sized (sized name <> content)
            where sized x = BS.fromStrict (toLEB128 @Natural (fromIntegral (BS.length x))) <> x
          withSections xs = foldl withCustomSection trivialWasmModule xs
      in
      [ testCase "public" $ do
          let mod = withSections [("icp:public test", "bar")]
          cid <- create
          ic_install ic00 (enum #install) cid mod ""
          cert <- getStateCert otherUser cid [["canister", cid, "metadata", "test"]]
          lookupPath (cert_tree cert) ["canister", cid, "metadata", "test"] @?= Found "bar"
          cert <- getStateCert anonymousUser cid [["canister", cid, "metadata", "test"]]
          lookupPath (cert_tree cert) ["canister", cid, "metadata", "test"] @?= Found "bar"
      , testCase "private" $ do
          let mod = withSections [("icp:private test", "bar")]
          cid <- create
          ic_install ic00 (enum #install) cid mod ""
          getStateCert' otherUser cid [["canister", cid, "metadata", "test"]] >>= code4xx
          getStateCert' anonymousUser cid [["canister", cid, "metadata", "test"]] >>= code4xx
          cert <- getStateCert defaultUser cid [["canister", cid, "metadata", "test"]]
          lookupPath (cert_tree cert) ["canister", cid, "metadata", "test"] @?= Found "bar"
      , testCase "duplicate public" $ do
          let mod = withSections [("icp:public test", "bar"), ("icp:public test", "baz")]
          cid <- create
          ic_install' ic00 (enum #install) cid mod "" >>= isReject [5]
      , testCase "duplicate private" $ do
          let mod = withSections [("icp:private test", "bar"), ("icp:private test", "baz")]
          cid <- create
          ic_install' ic00 (enum #install) cid mod "" >>= isReject [5]
      , testCase "duplicate mixed" $ do
          let mod = withSections [("icp:private test", "bar"), ("icp:public test", "baz")]
          cid <- create
          ic_install' ic00 (enum #install) cid mod "" >>= isReject [5]
      , testCase "invalid utf8 in module" $ do
          let mod = withSections [("icp:public \xe2\x28\xa1", "baz")]
          cid <- create
          ic_install' ic00 (enum #install) cid mod "" >>= isReject [5]
      , simpleTestCase "invalid utf8 in read_state" $ \cid -> do
          getStateCert' defaultUser cid [["canister", cid, "metadata", "\xe2\x28\xa1"]] >>= code4xx
      , testCase "unicode metadata name" $ do
          let mod = withSections [("icp:public ☃️", "bar")]
          cid <- create
          ic_install ic00 (enum #install) cid mod ""
          cert <- getStateCert anonymousUser cid [["canister", cid, "metadata", "☃️"]]
          lookupPath (cert_tree cert) ["canister", cid, "metadata", "☃️"] @?= Found "bar"
      , testCase "zero-length metadata name" $ do
          let mod = withSections [("icp:public ", "bar")]
          cid <- create
          ic_install ic00 (enum #install) cid mod ""
          cert <- getStateCert anonymousUser cid [["canister", cid, "metadata", ""]]
          lookupPath (cert_tree cert) ["canister", cid, "metadata", ""] @?= Found "bar"
      , testCase "metadata section name with spaces" $ do
          let mod = withSections [("icp:public metadata section name with spaces", "bar")]
          cid <- create
          ic_install ic00 (enum #install) cid mod ""
          cert <- getStateCert anonymousUser cid [["canister", cid, "metadata", "metadata section name with spaces"]]
          lookupPath (cert_tree cert) ["canister", cid, "metadata", "metadata section name with spaces"] @?= Found "bar"
      ]
    ]

  , testGroup "certified variables"
    [ simpleTestCase "initially empty" $ \cid -> do
      query cid (replyData getCertificate) >>= extractCertData cid >>= is ""
    , simpleTestCase "validates" $ \cid -> do
      query cid (replyData getCertificate)
        >>= decodeCert' >>= validateStateCert
    , simpleTestCase "present in query method (query call)" $ \cid -> do
      query cid (replyData (i2b getCertificatePresent))
        >>= is "\1\0\0\0"
    , simpleTestCase "not present in query method (update call)" $ \cid -> do
      callToQuery'' cid (replyData (i2b getCertificatePresent))
        >>= is2xx >>= isReply >>= is "\0\0\0\0"
    , simpleTestCase "not present in query method (inter-canister call)" $ \cid -> do
      do call cid $
          inter_call cid "query" defArgs {
            other_side = replyData (i2b getCertificatePresent)
          }
        >>= isRelay >>= isReply >>= is "\0\0\0\0"
    , simpleTestCase "not present in update method" $ \cid -> do
      call cid (replyData (i2b getCertificatePresent))
        >>= is "\0\0\0\0"

    , simpleTestCase "set and get" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> reply
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    , simpleTestCase "set twice" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> setCertifiedData "BAR" >>> reply
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "BAR"
    , simpleTestCase "set then trap" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> reply
      call' cid (setCertifiedData "BAR" >>> trap "Trapped") >>= isReject [5]
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    , simpleTestCase "too large traps, old value retained" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> reply
      call' cid (setCertifiedData (bytes (BS.replicate 33 0x42)) >>> reply)
        >>= isReject [5]
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    , testCase "set in init" $ do
      cid <- install $ setCertifiedData "FOO"
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    , testCase "set in pre-upgrade" $ do
      cid <- install $ onPreUpgrade (callback $ setCertifiedData "FOO")
      upgrade cid noop
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    , simpleTestCase "set in post-upgrade" $ \cid -> do
      upgrade cid $ setCertifiedData "FOO"
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    ]

  , testGroup "cycles" $
    let replyBalance = replyData (i64tob getBalance)
        replyBalance128 = replyData getBalance128
        rememberBalance =
          ignore (stableGrow (int 1)) >>>
          stableWrite (int 0) (i64tob getBalance)
        recallBalance = replyData (stableRead (int 0) (int 8))
        acceptAll = ignore (acceptCycles getAvailableCycles)
        queryBalance cid = query cid replyBalance >>= asWord64
        queryBalance128 cid = query cid replyBalance128 >>= asWord128

        -- At the time of writing, creating a canister needs at least 1T
        -- and the freezing limit is 5T
        -- (At some point, the max was 100T, but that is no longer the case)
        -- So lets try to stay away from these limits.
        -- The lowest denomination we deal with below is def_cycles`div`4
        def_cycles = 80_000_000_000_000 :: Word64

        -- The system burns cycles at unspecified rates. To cater for such behaviour,
        -- we make the assumption that no test burns more than the following epsilon.
        --
        -- The biggest fee we currenlty deal with is the system deducing 1T
        -- upon canister creation. So our epsilon needs to allow that and then
        -- some more.
        eps = 3_000_000_000_000 :: Integer

        isRoughly :: (HasCallStack, Show a, Num a, Integral a) => a -> a -> Assertion
        isRoughly exp act = assertBool
           (show act ++ " not near " ++ show exp)
           (abs (fromIntegral exp - fromIntegral act) < eps)

        create prog = do
          cid <- ic_provisional_create ic00 (Just (fromIntegral def_cycles)) empty
          installAt cid prog
          return cid
        create_via cid initial_cycles = do
          cid2 <- ic_create (ic00viaWithCycles cid initial_cycles) empty
          universal_wasm <- getTestWasm "universal_canister"
          ic_install (ic00via cid) (enum #install) cid2 universal_wasm (run noop)
          return cid2
    in
    [ testGroup "cycles API - backward compatibility" $
        [ simpleTestCase "canister_cycle_balance = canister_cycle_balance128 for numbers fitting in 64 bits" $ \cid -> do
          a <- queryBalance cid
          b <- queryBalance128 cid
          bothSame (a, fromIntegral b)
        , testCase "legacy API traps when a result is too big" $ do
          cid <- create noop
          let large = 2^(65::Int)
          ic_top_up ic00 cid large
          query' cid replyBalance >>= isReject [5]
          queryBalance128 cid >>= isRoughly (large + fromIntegral def_cycles)
        ]
    , testGroup "can use balance API" $
        let getBalanceTwice = join cat (i64tob getBalance)
            test = replyData getBalanceTwice
        in
        [ simpleTestCase "in query" $ \cid ->
          query cid test >>= as2Word64 >>= bothSame
        , simpleTestCase "in update" $ \cid ->
          call cid test >>= as2Word64 >>= bothSame
        , testCase "in init" $ do
          cid <- install (setGlobal getBalanceTwice)
          query cid (replyData getGlobal) >>= as2Word64 >>= bothSame
        , simpleTestCase "in callback" $ \cid ->
          call cid (inter_query cid defArgs{ on_reply = test }) >>= as2Word64 >>= bothSame
        ]
    , testGroup "can use available cycles API" $
        let getAvailableCyclesTwice = join cat (i64tob getAvailableCycles)
            test = replyData getAvailableCyclesTwice
        in
        [ simpleTestCase "in update" $ \cid ->
          call cid test >>= as2Word64 >>= bothSame
        , simpleTestCase "in callback" $ \cid ->
          call cid (inter_query cid defArgs{ on_reply = test }) >>= as2Word64 >>= bothSame
        ]
    , simpleTestCase "can accept zero cycles" $ \cid ->
        call cid (replyData (i64tob (acceptCycles (int64 0)))) >>= asWord64 >>= is 0
    , simpleTestCase "can accept more than available cycles" $ \cid ->
        call cid (replyData (i64tob (acceptCycles (int64 1)))) >>= asWord64 >>= is 0
    , simpleTestCase "can accept absurd amount of cycles" $ \cid ->
        call cid (replyData (acceptCycles128 (int64 maxBound) (int64 maxBound))) >>= asWord128 >>= is 0

    , testGroup "provisional_create_canister_with_cycles"
      [ testCase "balance as expected" $ do
        cid <- create noop
        queryBalance cid >>= isRoughly def_cycles

      , testCaseSteps "default (i.e. max) balance" $ \step -> do
        cid <- ic_provisional_create ic00 Nothing empty
        installAt cid noop
        cycles <- queryBalance128 cid
        step $ "Cycle balance now at " ++ show cycles

      , testCaseSteps "> 2^128 succeeds" $ \step -> do
        cid <- ic_provisional_create ic00 (Just (10 * 2^(128::Int))) empty
        installAt cid noop
        cycles <- queryBalance128 cid
        step $ "Cycle balance now at " ++ show cycles
      ]

    , testCase "cycles in canister_status" $ do
        cid <- create noop
        cs <- ic_canister_status ic00 cid
        isRoughly (fromIntegral def_cycles) (cs .! #cycles)

    , testGroup "cycle balance"
      [ testCase "install" $ do
        cid <- create rememberBalance
        query cid recallBalance >>= asWord64 >>= isRoughly def_cycles
      , testCase "update" $ do
        cid <- create noop
        call cid replyBalance >>= asWord64 >>= isRoughly def_cycles
      , testCase "query" $ do
        cid <- create noop
        query cid replyBalance >>= asWord64 >>= isRoughly def_cycles
      , testCase "in pre_upgrade" $ do
        cid <- create $ onPreUpgrade (callback rememberBalance)
        upgrade cid noop
        query cid recallBalance >>= asWord64 >>= isRoughly def_cycles
      , testCase "in post_upgrade" $ do
        cid <- create noop
        upgrade cid rememberBalance
        query cid recallBalance >>= asWord64 >>= isRoughly def_cycles
        queryBalance cid >>= isRoughly def_cycles
      ]
    , testCase "can send cycles" $ do
      cid1 <- create noop
      cid2 <- create noop
      do call cid1 $ inter_call cid2 "update" defArgs
          { other_side =
            replyDataAppend (i64tob getAvailableCycles) >>>
            acceptAll >>>
            reply
          , cycles = def_cycles `div` 4
          }
        >>= isRelay >>= isReply >>= asWord64 >>= isRoughly (def_cycles `div` 4)
      queryBalance cid1 >>= isRoughly (def_cycles - def_cycles `div` 4)
      queryBalance cid2 >>= isRoughly (def_cycles + def_cycles `div` 4)

    , testCase "sending more cycles than in balance traps" $ do
      cid <- create noop
      cycles <- queryBalance cid
      call' cid (inter_call cid cid defArgs { cycles = cycles + 1000_000 })
        >>= isReject [5]

    , testCase "relay cycles before accept traps" $ do
      cid1 <- create noop
      cid2 <- create noop
      cid3 <- create noop
      do call cid1 $ inter_call cid2 "update" defArgs
          { cycles = def_cycles `div` 2
          , other_side =
            inter_call cid3 "update" defArgs
              { other_side = acceptAll >>> reply
              , cycles = def_cycles + def_cycles `div` 4
              , on_reply = noop -- must not double reply
              } >>>
            acceptAll >>> reply
          , on_reply = trap "unexpected reply"
          , on_reject = replyData (i64tob getRefund)
          }
        >>= asWord64 >>= isRoughly (def_cycles `div` 2)
      queryBalance cid1 >>= isRoughly def_cycles
      queryBalance cid2 >>= isRoughly def_cycles
      queryBalance cid3 >>= isRoughly def_cycles
    , testCase "relay cycles after accept works" $ do
      cid1 <- create noop
      cid2 <- create noop
      cid3 <- create noop
      do call cid1 $ inter_call cid2 "update" defArgs
          { cycles = def_cycles `div` 2
          , other_side =
            acceptAll >>>
            inter_call cid3 "update" defArgs
              { other_side = acceptAll >>> reply
              , cycles = def_cycles + def_cycles `div` 4
              }
          , on_reply = replyData (i64tob getRefund)
          , on_reject = trap "unexpected reject"
          }
        >>= asWord64 >>= isRoughly 0
      queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
      queryBalance cid2 >>= isRoughly (def_cycles `div` 4)
      queryBalance cid3 >>= isRoughly (2*def_cycles + def_cycles `div` 4)
    , testCase "aborting call resets balance" $ do
      cid <- create noop
      (a,b) <- do
         call cid $
          callNew "Foo" "Bar" "baz" "quux" >>>
          callCyclesAdd (int64 (def_cycles `div` 2)) >>>
          replyDataAppend (i64tob getBalance) >>>
          callNew "Foo" "Bar" "baz" "quux" >>>
          replyDataAppend (i64tob getBalance) >>>
          reply
        >>= as2Word64
      isRoughly (def_cycles `div` 2) a
      isRoughly def_cycles b

    , testCase "partial refund" $ do
      cid1 <- create noop
      cid2 <- create noop
      do call cid1 $ inter_call cid2 "update" defArgs
          { cycles = def_cycles `div` 2
          , other_side = ignore (acceptCycles (int64 (def_cycles `div` 4))) >>> reply
          , on_reply = replyData (i64tob getRefund)
          , on_reject = trap "unexpected reject"
          }
        >>= asWord64 >>= isRoughly (def_cycles `div` 4)
      queryBalance cid1 >>= isRoughly (def_cycles - def_cycles `div` 4)
      queryBalance cid2 >>= isRoughly (def_cycles + def_cycles `div` 4)
    , testCase "cycles not in balance while in transit" $ do
      cid1 <- create noop
      do call cid1 $ inter_call cid1 "update" defArgs
          { cycles = def_cycles `div` 4
          , other_side = replyBalance
          , on_reject = trap "unexpected reject"
          }
        >>= isRelay >>= isReply >>= asWord64 >>= isRoughly (def_cycles - def_cycles `div` 4)
      queryBalance cid1 >>= isRoughly def_cycles
    , testCase "create and delete canister with cycles" $ do
      cid1 <- create noop
      cid2 <- create_via cid1 (def_cycles`div`2)
      queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
      queryBalance cid2 >>= isRoughly (def_cycles `div` 2)
      ic_stop_canister (ic00via cid1) cid2
      -- We load some cycles on the deletion call, just to check that they are refunded
      ic_delete_canister (ic00viaWithCycles cid1 (def_cycles`div`4)) cid2
      queryBalance cid1 >>= isRoughly (def_cycles`div`2)

    , testGroup "deposit_cycles"
      [ testCase "as controller" $ do
        cid1 <- create noop
        cid2 <- create_via cid1 (def_cycles`div`2)
        queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
        queryBalance cid2 >>= isRoughly (def_cycles `div` 2)
        ic_deposit_cycles (ic00viaWithCycles cid1 (def_cycles`div`4)) cid2
        queryBalance cid1 >>= isRoughly (def_cycles `div` 4)
        queryBalance cid2 >>= isRoughly (def_cycles - def_cycles `div` 4)
      , testCase "as other non-controlling canister" $ do
        cid1 <- create noop
        cid2 <- create_via cid1 (def_cycles`div`2)
        queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
        queryBalance cid2 >>= isRoughly (def_cycles `div` 2)
        ic_deposit_cycles (ic00viaWithCycles cid2 (def_cycles`div`4)) cid1
        queryBalance cid1 >>= isRoughly (def_cycles - def_cycles `div` 4)
        queryBalance cid2 >>= isRoughly (def_cycles `div` 4)
      , testCase "to non-existing canister" $ do
        cid1 <- create noop
        queryBalance cid1 >>= isRoughly def_cycles
        ic_deposit_cycles' (ic00viaWithCycles cid1 (def_cycles`div`4)) doesn'tExist
          >>= isReject [3,4,5]
        queryBalance cid1 >>= isRoughly def_cycles
      ]

    , testCase "two-step-refund" $ do
      cid1 <- create noop
      do call cid1 $ inter_call cid1 "update" defArgs
          { cycles = 10
          , other_side = inter_call cid1 "update" defArgs
              { cycles = 5
              , other_side = reply -- no accept
              , on_reply =
                    -- remember refund
                    replyDataAppend (i64tob getRefund) >>>
                    reply
              , on_reject = trap "unexpected reject"
              }
          , on_reply =
                -- remember the refund above and this refund
                replyDataAppend argData >>>
                replyDataAppend (i64tob getRefund) >>>
                reply
          , on_reject = trap "unexpected reject"
          }
        >>= as2Word64 >>= is (5,10)
      queryBalance cid1 >>= isRoughly def_cycles -- nothing lost?

    , testGroup "provisional top up"
      [ testCase "as user" $ do
        cid <- create noop
        queryBalance cid >>= isRoughly def_cycles
        ic_top_up ic00 cid (fromIntegral def_cycles)
        queryBalance cid >>= isRoughly (2 * def_cycles)
      , testCase "as self" $ do
        cid <- create noop
        queryBalance cid >>= isRoughly def_cycles
        ic_top_up (ic00via cid) cid (fromIntegral def_cycles)
        queryBalance cid >>= isRoughly (2 * def_cycles)
      , testCase "as other canister" $ do
        cid <- create noop
        cid2 <- create noop
        queryBalance cid >>= isRoughly def_cycles
        ic_top_up (ic00via cid2) cid (fromIntegral def_cycles)
        queryBalance cid >>= isRoughly (2 * def_cycles)
      , testCaseSteps "more than 2^128" $ \step -> do
        cid <- create noop
        ic_top_up ic00 cid (10 * 2^(128::Int))
        cycles <- queryBalance128 cid
        step $ "Cycle balance now at " ++ show cycles
      , testCase "nonexisting canister" $ do
        ic_top_up' ic00 doesn'tExist (fromIntegral def_cycles)
          >>= isReject [3,5]
      ]
    ]

  , testGroup "canister_inspect_message"
    [ testCase "empty canister" $ do
      cid <- create
      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []

    , testCase "accept all" $ do
      cid <- install $ onInspectMessage $ callback acceptMessage
      call_ cid reply
      callToQuery'' cid reply >>= is2xx >>= isReply >>= is ""

    , testCase "no accept_message" $ do
      cid <- install $ onInspectMessage $ callback noop
      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []
      -- check that inter-canister calls still work
      cid2 <- install noop
      call cid2 (inter_update cid defArgs)
        >>= isRelay >>= isReply >>= is ("Hello " <> cid2 <> " this is " <> cid)

    , testCase "two calls to accept_message" $ do
      cid <- install $ onInspectMessage $ callback $ acceptMessage >>> acceptMessage
      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []

    , testCase "trap" $ do
      cid <- install $ onInspectMessage $ callback $ trap "no no no"
      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []

    , testCase "method_name correct" $ do
      cid <- install $ onInspectMessage $ callback $
        trapIfEq methodName "update" "no no no" >>> acceptMessage

      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= is2xx >>= isReply >>= is ""

    , testCase "caller correct" $ do
      cid <- install $ onInspectMessage $ callback $
        trapIfEq caller (bytes defaultUser) "no no no" >>> acceptMessage

      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []

      awaitCall' cid (callRequestAs otherUser cid reply)
        >>= is2xx >>= isReply >>= is ""
      awaitCall' cid (callToQueryRequestAs otherUser cid reply)
        >>= is2xx >>= isReply >>= is ""

    , testCase "arg correct" $ do
      cid <- install $ onInspectMessage $ callback $
        trapIfEq argData (callback reply) "no no no" >>> acceptMessage

      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []

      call cid (replyData "foo") >>= is "foo"
      callToQuery'' cid (replyData "foo") >>= is2xx >>= isReply >>= is "foo"

    , testCase "management canister: raw_rand not accepted" $ do
      ic_raw_rand'' defaultUser >>= isErrOrReject []

    , testCase "management canister: http_request not accepted" $ do
      ic_http_request'' defaultUser >>= isErrOrReject []

    , testCase "management canister: ecdsa_public_key not accepted" $ do
      ic_ecdsa_public_key'' defaultUser >>= isErrOrReject []

    , testCase "management canister: sign_with_ecdsa not accepted" $ do
      ic_sign_with_ecdsa'' defaultUser (sha256 "dummy") >>= isErrOrReject []

    , simpleTestCase "management canister: deposit_cycles not accepted" $ \cid -> do
      ic_deposit_cycles'' defaultUser cid >>= isErrOrReject []

    , simpleTestCase "management canister: wrong sender not accepted" $ \cid -> do
      ic_canister_status'' otherUser cid >>= isErrOrReject []
    ]

  , testGroup "Delegation targets" $ let
      callReq cid = rec
        [ "request_type" =: GText "call"
        , "sender" =: GBlob defaultUser
        , "canister_id" =: GBlob cid
        , "method_name" =: GText "update"
        , "arg" =: GBlob (run reply)
        ]

      mgmtReq cid = rec
        [ "request_type" =: GText "call"
        , "sender" =: GBlob defaultUser
        , "canister_id" =: GBlob ""
        , "method_name" =: GText "canister_status"
        , "arg" =: GBlob (Candid.encode (#canister_id .== Principal cid))
        ]

      good cid req dels = do
        req <- addExpiry req
        let rid = requestId req
        -- sign request with delegations
        delegationEnv defaultSK dels req >>= postCallCBOR cid >>= code2xx
        -- wait for it
        void $ awaitStatus (getRequestStatus defaultUser cid rid) >>= isReply
        -- also read status with delegation
        sreq <- addExpiry $ rec
          [ "request_type" =: GText "read_state"
          , "sender" =: GBlob defaultUser
          , "paths" =: GList [GList [GBlob "request_status", GBlob rid]]
          ]
        delegationEnv defaultSK dels sreq >>= postReadStateCBOR cid >>= void . code2xx

      badSubmit cid req dels = do
        req <- addExpiry req
        -- sign request with delegations (should fail)
        delegationEnv defaultSK dels req >>= postCallCBOR cid >>= code4xx

      badRead cid req dels = do
        req <- addExpiry req
        let rid = requestId req
        -- submit with plain signature
        envelope defaultSK req >>= postCallCBOR cid >>= code202
        -- wait for it
        void $ awaitStatus (getRequestStatus defaultUser cid rid) >>= isReply
        -- also read status with delegation
        sreq <- addExpiry $ rec
          [ "request_type" =: GText "read_state"
          , "sender" =: GBlob defaultUser
          , "paths" =: GList [GList [GBlob "request_status", GBlob rid]]
          ]
        delegationEnv defaultSK dels sreq >>= postReadStateCBOR cid >>= void . code4xx

      goodTestCase name mkReq mkDels =
        simpleTestCase name $ \cid -> good cid (mkReq cid) (mkDels cid)

      badTestCase name mkReq mkDels = testGroup name
        [ simpleTestCase "in submit" $ \cid -> badSubmit cid (mkReq cid) (mkDels cid)
        , simpleTestCase "in read_state" $ \cid -> badRead cid (mkReq cid) (mkDels cid)
        ]

      withEd25519 = zip [createSecretKeyEd25519 (BS.singleton n) | n <- [0..]]
      withWebAuthnECDSA = zip [createSecretKeyWebAuthnECDSA (BS.singleton n) | n <- [0..]]
      withWebAuthnRSA = zip [createSecretKeyWebAuthnRSA (BS.singleton n) | n <- [0..]]

    in
    [ goodTestCase "one delegation, singleton target" callReq $ \cid ->
      withEd25519 [Just [cid]]
    , badTestCase "one delegation, wrong singleton target" callReq $ \_cid ->
      withEd25519 [Just [doesn'tExist]]
    , goodTestCase "one delegation, two targets" callReq $ \cid ->
      withEd25519 [Just [cid, doesn'tExist]]
    , goodTestCase "two delegations, two targets, webauthn ECDSA" callReq $ \cid ->
      withWebAuthnECDSA [Just [cid, doesn'tExist], Just [cid, doesn'tExist]]
    , goodTestCase "two delegations, two targets, webauthn RSA" callReq $ \cid ->
      withWebAuthnRSA [Just [cid, doesn'tExist], Just [cid, doesn'tExist]]
    , goodTestCase "one delegation, redundant targets" callReq $ \cid ->
      withEd25519 [Just [cid, cid, doesn'tExist]]
    , goodTestCase "two delegations, singletons" callReq $ \cid ->
      withEd25519 [Just [cid], Just [cid] ]
    , goodTestCase "two delegations, first restricted" callReq $ \cid ->
      withEd25519 [Just [cid], Nothing ]
    , goodTestCase "two delegations, second restricted" callReq $ \cid ->
      withEd25519 [Nothing, Just [cid]]
    , badTestCase "two delegations, empty intersection" callReq $ \cid ->
      withEd25519 [Just [cid], Just [doesn'tExist]]
    , badTestCase "two delegations, first empty target set" callReq $ \cid ->
      withEd25519 [Just [], Just [cid]]
    , badTestCase "two delegations, second empty target set" callReq $ \cid ->
      withEd25519 [Just [cid], Just []]
    , goodTestCase "management canister: correct target" mgmtReq $ \_cid ->
      withEd25519 [Just [""]]
    , badTestCase "management canister: empty target set" mgmtReq $ \_cid ->
      withEd25519 [Just []]
    , badTestCase "management canister: bogus target" mgmtReq $ \_cid ->
      withEd25519 [Just [doesn'tExist]]
    , badTestCase "management canister: bogus target (using target canister)" mgmtReq $ \cid ->
      withEd25519 [Just [cid]]
    ]

  , testGroup "Authentication schemes" $
    let ed25519SK2 = createSecretKeyEd25519 "more keys"
        ed25519SK3 = createSecretKeyEd25519 "yet more keys"
        ed25519SK4 = createSecretKeyEd25519 "even more keys"
        delEnv sks = delegationEnv otherSK (map (, Nothing) sks) -- no targets in these tests
    in flip foldMap
      [ ("Ed25519",            otherUser,         envelope otherSK)
      , ("ECDSA",              ecdsaUser,         envelope ecdsaSK)
      , ("secp256k1",          secp256k1User,     envelope secp256k1SK)
      , ("WebAuthn ECDSA",     webAuthnECDSAUser, envelope webAuthnECDSASK)
      , ("WebAuthn RSA",       webAuthnRSAUser,   envelope webAuthnRSASK)
      , ("empty delegations",  otherUser,         delEnv [])
      , ("same delegations",   otherUser,         delEnv [otherSK])
      , ("three delegations",  otherUser,         delEnv [ed25519SK2, ed25519SK3])
      , ("four delegations",   otherUser,         delEnv [ed25519SK2, ed25519SK3, ed25519SK4])
      , ("mixed delegations",  otherUser,         delEnv [defaultSK, webAuthnECDSASK, webAuthnRSASK, ecdsaSK, secp256k1SK])
      ] $ \ (name, user, env) ->
    [ simpleTestCase (name ++ " in query") $ \cid -> do
      req <- addExpiry $ rec
            [ "request_type" =: GText "query"
            , "sender" =: GBlob user
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "query"
            , "arg" =: GBlob (run reply)
            ]
      signed_req <- env req
      postQueryCBOR cid signed_req >>= okCBOR >>= queryResponse >>= isReply >>= is ""

    , simpleTestCase (name ++ " in update") $ \cid -> do
      req <- addExpiry $ rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob user
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "update"
            , "arg" =: GBlob (run reply)
            ]
      signed_req <- env req
      postCallCBOR cid signed_req >>= code2xx

      awaitStatus (getRequestStatus user cid (requestId req)) >>= isReply >>= is ""
    ]

  , testGroup "signature checking" $
    [ ("with bad signature", return . badEnvelope, id)
    , ("with wrong key", envelope otherSK, id)
    , ("with empty domain separator", noDomainSepEnv defaultSK, id)
    , ("with no expiry", envelope defaultSK, noExpiryEnv)
    , ("with expiry in the past", envelope defaultSK, pastExpiryEnv)
    , ("with expiry in the future", envelope defaultSK, futureExpiryEnv)
    ] <&> \(name, env, mod_req) -> testGroup name
      [ simpleTestCase "in query" $ \cid -> do
        good_req <- addNonce >=> addExpiry $ rec
              [ "request_type" =: GText "query"
              , "sender" =: GBlob defaultUser
              , "canister_id" =: GBlob cid
              , "method_name" =: GText "query"
              , "arg" =: GBlob (run reply)
              ]
        queryCBOR cid good_req >>= queryResponse >>= isReply >>= is ""
        env (mod_req good_req) >>= postQueryCBOR cid >>= code4xx

      , simpleTestCase "in empty read state request" $ \cid -> do
          good_req <- addNonce >=> addExpiry $ readStateEmpty
          envelope defaultSK good_req >>= postReadStateCBOR cid >>= code2xx
          env (mod_req good_req) >>= postReadStateCBOR cid >>= code4xx

      , simpleTestCase "in call" $ \cid -> do
          good_req <- addNonce >=> addExpiry $ rec
                [ "request_type" =: GText "call"
                , "sender" =: GBlob defaultUser
                , "canister_id" =: GBlob cid
                , "method_name" =: GText "query"
                , "arg" =: GBlob (run reply)
                ]
          let req = mod_req good_req
          env req >>= postCallCBOR cid >>= code202_or_4xx

          -- Also check that the request was not created
          ingressDelay
          getRequestStatus defaultUser cid (requestId req) >>= is UnknownStatus

          -- check that with a valid signature, this would have worked
          awaitCall cid good_req >>= isReply >>= is ""
      ]

  , testGroup "Canister signatures" $
    let genId cid seed =
          DER.encode DER.CanisterSig $ CanisterSig.genPublicKey (EntityId cid) seed

        genSig cid seed msg = do
          -- Create the tree
          let tree = construct $
                SubTrees $ M.singleton "sig" $
                SubTrees $ M.singleton (sha256 seed) $
                SubTrees $ M.singleton (sha256 msg) $
                Value ""
          -- Store it as certified data
          call_ cid (setCertifiedData (bytes (reconstruct tree)) >>> reply)
          -- Get certificate
          cert <- query cid (replyData getCertificate) >>= decodeCert'
          -- double check it certifies
          validateStateCert cert
          certValue cert ["canister", cid, "certified_data"] >>= is (reconstruct tree)

          return $ CanisterSig.genSig cert tree

        exampleQuery cid userKey = addExpiry $ rec
          [ "request_type" =: GText "query"
          , "sender" =: GBlob (mkSelfAuthenticatingId userKey)
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "query"
          , "arg" =: GBlob (run (replyData "It works!"))
          ]
        simpleEnv userKey sig req delegations = rec $
          [ "sender_pubkey" =: GBlob userKey
          , "sender_sig" =: GBlob sig
          , "content" =: req
          ] ++
          [ "sender_delegation" =: GList delegations | not (null delegations) ]
    in
    [ simpleTestCase "direct signature" $ \cid -> do
      let userKey = genId cid "Hello!"
      req <- exampleQuery cid userKey
      sig <- genSig cid "Hello!" $ "\x0Aic-request" <> requestId req
      let env = simpleEnv userKey sig req []
      postQueryCBOR cid env >>= okCBOR >>= queryResponse >>= isReply >>= is "It works!"

    , simpleTestCase "direct signature (empty seed)" $ \cid -> do
      let userKey = genId cid ""
      req <- exampleQuery cid userKey
      sig <- genSig cid "" $ "\x0Aic-request" <> requestId req
      let env = simpleEnv userKey sig req []
      postQueryCBOR cid env >>= okCBOR >>= queryResponse >>= isReply >>= is "It works!"

    , simpleTestCase "direct signature (wrong seed)" $ \cid -> do
      let userKey = genId cid "Hello"
      req <- exampleQuery cid userKey
      sig <- genSig cid "Hullo" $ "\x0Aic-request" <> requestId req
      let env = simpleEnv userKey sig req []
      postQueryCBOR cid env >>= code4xx

    , simpleTestCase "direct signature (wrong cid)" $ \cid -> do
      let userKey = genId doesn'tExist "Hello"
      req <- exampleQuery cid userKey
      sig <- genSig cid "Hello" $ "\x0Aic-request" <> requestId req
      let env = simpleEnv userKey sig req []
      postQueryCBOR cid env >>= code4xx

    , simpleTestCase "direct signature (wrong root key)" $ \cid -> do
      let seed = "Hello"
      let userKey = genId cid seed
      req <- exampleQuery cid userKey
      let msg = "\x0Aic-request" <> requestId req
      -- Create the tree
      let tree = construct $
            SubTrees $ M.singleton "sig" $
            SubTrees $ M.singleton (sha256 seed) $
            SubTrees $ M.singleton (sha256 msg) $
            Value ""
      -- Create a fake certificate
      let cert_tree = construct $
            SubTrees $ M.singleton "canister" $
            SubTrees $ M.singleton cid $
            SubTrees $ M.singleton "certified_data" $
            Value (reconstruct tree)
      let fake_root_key = createSecretKeyBLS "not the root key"
      cert_sig <- sign "ic-state-root" fake_root_key (reconstruct cert_tree)
      let cert = Certificate { cert_tree, cert_sig, cert_delegation = Nothing }
      let sig = CanisterSig.genSig cert tree
      let env = simpleEnv userKey sig req []
      postQueryCBOR cid env >>= code4xx

    , simpleTestCase "delegation to Ed25519" $ \cid -> do
      let userKey = genId cid "Hello!"

      t <- getPOSIXTime
      let expiry = round ((t + 60) * 1000_000_000)
      let delegation = rec
            [ "pubkey" =: GBlob (toPublicKey otherSK)
            , "expiration" =: GNat expiry
            ]
      sig <- genSig cid "Hello!" $ "\x1Aic-request-auth-delegation" <> requestId delegation
      let signed_delegation = rec [ "delegation" =: delegation, "signature" =: GBlob sig ]

      req <- exampleQuery cid userKey
      sig <- sign "ic-request" otherSK (requestId req)
      let env = simpleEnv userKey sig req [signed_delegation]
      postQueryCBOR cid env >>= okCBOR >>= queryResponse >>= isReply >>= is "It works!"

    , simpleTestCase "delegation from Ed25519" $ \cid -> do
      let userKey = genId cid "Hello!"

      t <- getPOSIXTime
      let expiry = round ((t + 60) * 1000_000_000)
      let delegation = rec
            [ "pubkey" =: GBlob userKey
            , "expiration" =: GNat expiry
            ]
      sig <- sign "ic-request-auth-delegation" otherSK (requestId delegation)
      let signed_delegation = rec [ "delegation" =: delegation, "signature" =: GBlob sig ]

      req <- addExpiry $ rec
          [ "request_type" =: GText "query"
          , "sender" =: GBlob otherUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "query"
          , "arg" =: GBlob (run (replyData "It works!"))
          ]
      sig <- genSig cid "Hello!" $ "\x0Aic-request" <> requestId req
      let env = simpleEnv (toPublicKey otherSK) sig req [signed_delegation]
      postQueryCBOR cid env >>= okCBOR >>= queryResponse >>= isReply >>= is "It works!"

    ]
  ]
