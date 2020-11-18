{- |

This module contains a test suite for the Internet Computer

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IC.Test.Spec (preFlight, TestConfig, icTests) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Text.Hex as H
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Set as S
import Network.HTTP.Client
import Network.HTTP.Types
import Numeric.Natural
import Data.List
import Data.Char
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options
import Control.Monad.Trans
import Control.Concurrent
import Control.Monad
import Control.Exception (catch)
import Data.Traversable
import Data.Word
import Data.Functor
import GHC.TypeLits
import System.FilePath
import System.Directory
import System.Environment
import System.Random
import System.Exit
import Data.Time.Clock.POSIX
import Codec.Candid (Principal(..))
import qualified Data.Binary.Get as Get
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
import qualified IC.Crypto.DER as DER
import IC.Id.Forms hiding (Blob)
import IC.Test.Options
import IC.Test.Universal
import IC.HashTree hiding (Blob, Label)
import IC.Certificate
import IC.Certificate.Value
import IC.Certificate.CBOR
import IC.Hash

-- * Test data, standard requests

doesn'tExist :: Blob
doesn'tExist = "\xDE\xAD\xBE\xEF" -- hopefully no such canister/user exists

defaultSK :: SecretKey
defaultSK = createSecretKeyEd25519Raw "fixed32byteseedfortesting"

otherSK :: SecretKey
otherSK = createSecretKeyEd25519Raw "anotherfixed32byteseedfortesting"

ed25519SK :: SecretKey
ed25519SK = createSecretKeyEd25519 "notarawkey"

webAuthnSK :: SecretKey
webAuthnSK = createSecretKeyWebAuthn "webauthnseed"

ecdsaSK :: SecretKey
ecdsaSK = createSecretKeyWebAuthn "ecdsaseed"

defaultUser :: Blob
defaultUser = mkSelfAuthenticatingId $ toPublicKey defaultSK
otherUser :: Blob
otherUser = mkSelfAuthenticatingId $ toPublicKey otherSK
webAuthnUser :: Blob
webAuthnUser = mkSelfAuthenticatingId $ toPublicKey webAuthnSK
ed25519User :: Blob
ed25519User = mkSelfAuthenticatingId $ toPublicKey ed25519SK
ecdsaUser :: Blob
ecdsaUser = mkSelfAuthenticatingId $ toPublicKey ecdsaSK
anonymousUser :: Blob
anonymousUser = "\x04"

queryToNonExistant :: GenR
queryToNonExistant = rec
    [ "request_type" =: GText "query"
    , "sender" =: GBlob anonymousUser
    , "canister_id" =: GBlob doesn'tExist
    , "method_name" =: GText "foo"
    , "arg" =: GBlob "nothing to see here"
    ]

readStateEmpty :: GenR
readStateEmpty = rec
    [ "request_type" =: GText "read_state"
    , "sender" =: GBlob defaultUser
    , "paths" =: GList []
    ]


trivialWasmModule :: Blob
trivialWasmModule = "\0asm\1\0\0\0"

addIfNotThere :: Monad m => T.Text -> m GenR -> GenR -> m GenR
addIfNotThere f _ (GRec hm)| f `HM.member` hm = return (GRec hm)
addIfNotThere f a (GRec hm) = do
  x <- a
  return $ GRec $ HM.insert f x hm
addIfNotThere _ _ _ = error "addIfNotThere: not a record"

deleteField :: T.Text -> GenR -> GenR
deleteField f (GRec hm) = GRec $ HM.delete f hm
deleteField _ _ = error "deleteField: not a record"

modNatField :: T.Text -> (Natural -> Natural) -> GenR -> GenR
modNatField f g (GRec hm) = GRec $ HM.adjust underNat f hm
  where underNat :: GenR -> GenR
        underNat (GNat n) = GNat (g n)
        underNat _ = error "modNatField: not a nat field"
modNatField _ _ _ = error "modNatField: not a record"

addNonce :: GenR -> IO GenR
addNonce = addIfNotThere "nonce" $
    GBlob <$> getRand8Bytes

-- Adds expiry 1 minute
addExpiry :: GenR -> IO GenR
addExpiry = addIfNotThere "ingress_expiry" $ do
    t <- getPOSIXTime
    return $ GNat $ round ((t + 60) * 1000_000_000)

envelopeFor :: Blob -> GenR -> IO GenR
envelopeFor u content | u == anonymousUser = return $ rec [ "content" =: content ]
envelopeFor u content = envelope key content
  where
    key ::  SecretKey
    key | u == defaultUser = defaultSK
        | u == otherUser = otherSK
        | u == ed25519User = ed25519SK
        | u == webAuthnUser = webAuthnSK
        | u == ecdsaUser = ecdsaSK
        | u == anonymousUser = error "No key for the anonymous user"
        | otherwise = error $ "Don't know key for user " ++ show u

envelope :: SecretKey -> GenR -> IO GenR
envelope sk = delegationEnv sk []

delegationEnv :: SecretKey -> [(SecretKey, Maybe [Blob])] -> GenR -> IO GenR
delegationEnv sk1 dels content = do
    let sks = sk1 : map fst dels

    t <- getPOSIXTime
    let expiry = round ((t + 60) * 1000_000_000)
    delegations <- for (zip sks dels) $ \(sk1, (sk2,targets)) -> do
      let delegation = rec $
            [ "pubkey" =: GBlob (toPublicKey sk2)
            , "expiration" =: GNat expiry
            ] ++
            [ "targets" =: GList (map GBlob ts) | Just ts <- pure targets ]
      sig <- sign "ic-request-auth-delegation" sk1 (requestId delegation)
      return $ rec [ "delegation" =: delegation, "signature" =: GBlob sig ]
    sig <- sign "ic-request" (last sks) (requestId content)
    return $ rec $
      [ "sender_pubkey" =: GBlob (toPublicKey sk1)
      , "sender_sig" =: GBlob sig
      , "content" =: content
      ] ++
      [ "sender_delegation" =: GList delegations | not (null delegations) ]

-- a little bit of smartness in our combinators

senderOf :: GenR -> Blob
senderOf (GRec hm) | Just (GBlob id) <- HM.lookup "sender" hm = id
senderOf _ = anonymousUser

badEnvelope :: GenR -> GenR
badEnvelope content = rec
    [ "sender_pubkey" =: GBlob (toPublicKey defaultSK)
    , "sender_sig" =: GBlob (BS.replicate 64 0x42)
    , "content" =: content
    ]

noDomainSepEnv :: SecretKey -> GenR -> IO GenR
noDomainSepEnv sk content = do
  sig <- sign "" sk (requestId content)
  return $ rec
    [ "sender_pubkey" =: GBlob (toPublicKey sk)
    , "sender_sig" =: GBlob sig
    , "content" =: content
    ]

noExpiryEnv, pastExpiryEnv, futureExpiryEnv :: GenR -> GenR
noExpiryEnv = deleteField "ingress_expiry"
pastExpiryEnv = modNatField "ingress_expiry" (subtract 3600_000_000_000)
futureExpiryEnv = modNatField "ingress_expiry" (+ 3600_000_000_000)

-- * Preflight checks: Get the root key, and tell user about versions

data TestConfig = TestConfig
    { tc_root_key :: Blob
    , tc_manager :: Manager
    , tc_endPoint :: String
    }

preFlight :: OptionSet -> IO TestConfig
preFlight os = do
    let Endpoint ep = lookupOption os
    manager <- newManager defaultManagerSettings
    request <- parseRequest $ ep ++ "/api/v1/status"
    putStrLn $ "Fetching endpoint status from " ++ show ep ++ "..."
    s <- (httpLbs request manager >>= okCBOR >>= statusResonse)
        `catch` (\(HUnitFailure _ r) -> putStrLn r >> exitFailure)

    putStrLn $ "Spec version tested:  " ++ T.unpack specVersion
    putStrLn $ "Spec version claimed: " ++ T.unpack (status_api_version s)

    return TestConfig
        { tc_root_key = status_root_key s
        , tc_manager = manager
        , tc_endPoint = ep
        }


-- * The actual test suite (see below for helper functions)

icTests :: TestConfig -> TestTree
icTests = withTestConfig $ testGroup "Public Spec acceptance tests"
  [ simpleTestCase "create and install" $ \_ ->
      return ()

  , testCase "create_canister necessary" $
      ic_install' ic00 (enum #install) doesn'tExist trivialWasmModule ""
          >>= isReject [3,5]

  , testCaseSteps "management requests" $ \step -> do
      step "Create"
      can_id <- ic_create ic00

      step "Install"
      ic_install ic00 (enum #install) can_id trivialWasmModule ""

      step "Install again fails"
      ic_install' ic00 (enum #install) can_id trivialWasmModule ""
        >>= isReject [3,5]

      step "Reinstall"
      ic_install ic00 (enum #reinstall) can_id trivialWasmModule ""

      step "Reinstall as wrong user"
      ic_install' (ic00as otherUser) (enum #reinstall) can_id trivialWasmModule ""
        >>= isReject [3,5]

      step "Upgrade"
      ic_install ic00 (enum #upgrade) can_id trivialWasmModule ""

      step "Upgrade as wrong user"
      ic_install'  (ic00as otherUser) (enum #upgrade) can_id trivialWasmModule ""
        >>= isReject [3,5]

      step "Change controller"
      ic_set_controller ic00 can_id otherUser

      step "Change controller (with wrong controller)"
      ic_set_controller' ic00 can_id otherUser
        >>= isReject [3,5]

      step "Reinstall as new controller"
      ic_install (ic00as otherUser) (enum #reinstall) can_id trivialWasmModule ""

  , testCaseSteps "reinstall on empty" $ \step -> do
      step "Create"
      can_id <- ic_create ic00

      step "Reinstall over empty canister"
      ic_install ic00 (enum #reinstall) can_id trivialWasmModule ""

  , testCaseSteps "canister_status" $ \step -> do
      step "Create empty"
      cid <- ic_create ic00
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running
      cs .! #controller @?= Principal defaultUser
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
      call' cid reply >>= isReject [5]

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

  , testCaseSteps "canister deletion" $ \step -> do
      cid <- install noop

      step "Deletion fails"
      ic_delete_canister' ic00 cid >>= isReject [5]

      step "Stop"
      ic_stop_canister ic00 cid

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
      do call' cid2 $ inter_update cid defArgs
        >>= isRelayReject [3]

      step "Cannot call (query)?"
      query' cid reply >>= isReject [3]

      step "Cannot query canister_status"
      ic_canister_status' ic00 cid >>= isReject [3,5]

      step "Deletion fails"
      ic_delete_canister' ic00 cid >>= isReject [3,5]


  , testCaseSteps "canister lifecycle (wrong controller)" $ \step -> do
      cid <- install noop

      step "Start as wrong user"
      ic_start_canister' (ic00as otherUser) cid >>= isReject [3,5]
      step "Stop as wrong user"
      ic_stop_canister' (ic00as otherUser) cid >>= isReject [3,5]
      step "Canister Status as wrong user"
      ic_canister_status' (ic00as otherUser) cid >>= isReject [3,5]
      step "Delete as wrong user"
      ic_delete_canister' (ic00as otherUser) cid >>= isReject [3,5]


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
      >>= isRelayReject [3,5]

    step "Reinstall"
    ic_install (ic00via cid) (enum #reinstall) can_id trivialWasmModule ""

    step "Reinstall as wrong user"
    ic_install' (ic00via cid2) (enum #reinstall) can_id trivialWasmModule ""
      >>= isRelayReject [3,5]

    step "Upgrade"
    ic_install (ic00via cid) (enum #upgrade) can_id trivialWasmModule ""

    step "Change controller"
    ic_set_controller (ic00via cid) can_id cid2

    step "Change controller (with wrong controller)"
    ic_set_controller' (ic00via cid) can_id cid2
      >>= isRelayReject [3,5]

    step "Reinstall as new controller"
    ic_install (ic00via cid2) (enum #reinstall) can_id trivialWasmModule ""

    step "Create"
    can_id2 <- ic_create (ic00via cid)

    step "Reinstall on empty"
    ic_install (ic00via cid) (enum #reinstall) can_id2 trivialWasmModule ""

  , simpleTestCase "aaaaa-aa (inter-canister, large)" $ \cid -> do
    universal_wasm <- getTestWasm "universal_canister"
    can_id <- ic_create (ic00via cid)
    ic_install (ic00via cid) (enum #install) can_id universal_wasm ""
    do call can_id $ replyData "Hi"
      >>= is "Hi"

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
    [ simpleTestCase "Call" $ \cid ->
      call cid (replyData "ABCD") >>= is "ABCD"

    , simpleTestCase "Call (query)" $ \cid -> do
      query cid (replyData "ABCD") >>= is "ABCD"

    , simpleTestCase "Call no non-existant update method" $ \cid ->
      do submitCBOR $ rec
          [ "request_type" =: GText "call"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "no_such_update"
          , "arg" =: GBlob ""
          ]
        >>= isReject [3]

    , simpleTestCase "Call no non-existant query method" $ \cid ->
      do readCBOR $ rec
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
      call' cid (setGlobal "FOO") >>= isReject [5]
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

  , testGroup "anonymous user"
    [ simpleTestCase "update, sender absent fails" $ \cid ->
      do envelopeFor anonymousUser $ rec
          [ "request_type" =: GText "call"
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "update"
          , "arg" =: GBlob (run (replyData caller))
          ]
        >>= postCBOR "/api/v1/submit" >>= code4xx
    , simpleTestCase "query, sender absent fails" $ \cid ->
      do envelopeFor anonymousUser $ rec
          [ "request_type" =: GText "query"
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "query"
          , "arg" =: GBlob (run (replyData caller))
          ]
        >>= postCBOR "/api/v1/read" >>= code4xx
    , simpleTestCase "update, sender explicit" $ \cid ->
      do submitCBOR $ rec
          [ "request_type" =: GText "call"
          , "canister_id" =: GBlob cid
          , "sender" =: GBlob anonymousUser
          , "method_name" =: GText "update"
          , "arg" =: GBlob (run (replyData caller))
          ]
        >>= isReply >>= is anonymousUser
    , simpleTestCase "query, sender explicit" $ \cid ->
      do readCBOR $ rec
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

  , testGroup "API availablility" $
    {-
    This section checks various API calls in various contexts, to see
    if they trap when they should
    This mirros the table in https://docs.dfinity.systems/public/#system-api-imports

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
            cid <- ic_create ic00
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
        , "U" =: reqResponse (\prog -> do
            cid <- install noop
            call' cid (prog >>> reply)
          )
        , "Q" =: reqResponse (\prog -> do
            cid <- install noop
            query' cid (prog >>> reply)
          )
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
        ]

      -- context builder helpers
      reqResponse act = (act >=> void . isReply, act >=> isReject [5])
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

      star = "I G U Q Ry Rt"
      never = ""

    in concat
    [ t "msg_arg_data"           "I U Q Ry"  $ ignore argData
    , t "msg_caller"             "I G U Q"   $ ignore caller
    , t "msg_reject_code"        "Ry Rt"     $ ignore reject_code
    , t "msg_reject_msg"         "Rt"        $ ignore reject_msg
    , t "msg_reply_data_append"  "U Q Ry Rt" $ replyDataAppend "Hey!"
    , t "msg_reply"              never         reply -- due to double reply
    , t "msg_reject"             never       $ reject "rejecting" -- due to double reply
    , t "msg_cycles_available"   "U Rt Ry"   $ ignore getAvailableCycles
    , t "msg_cycles_refunded"    "Rt Ry"     $ ignore getRefund
    , t "msg_cycles_accept"      "U Rt Ry"   $ ignore (acceptCycles (int64 0))
    , t "canister_self"          star        $ ignore self
    , t "canister_cycle_balance" star        $ ignore getBalance
    , t "call_new…call_perform" "U Rt Ry"   $
        callNew "foo" "bar" "baz" "quux" >>>
        callDataAppend "foo" >>>
        callCyclesAdd (int64 0) >>>
        callPerform
    , t "call_data_append"       never       $ callDataAppend (bytes "foo")
    , t "call_cycles_add"        never       $ callCyclesAdd (int64 0)
    , t "call_perform"           never         callPerform
    , t "stable_size"            star        $ ignore stableSize
    , t "stable_grow"            star        $ ignore $ stableGrow (int 1)
    , t "stable_read"            star        $ ignore $ stableRead (int 0) (int 0)
    , t "stable_write"           star        $ stableWrite (int 0) ""
    , t "time"                   star        $ ignore getTime
    , t "debug_print"            star        $ debugPrint "hello"
    , t "trap"                   never       $ trap "this better traps"
    ]

  , simpleTestCase "self" $ \cid ->
    query cid (replyData self) >>= is cid

  , testGroup "inter-canister calls"
    [ testGroup "builder interface"
      [ testGroup "traps without call_new"
        [ simpleTestCase "call_data_append" $ \cid ->
          call' cid (callDataAppend "Foo" >>> reply) >>= isReject [5]
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
          >>= is ("Hello " <> cid <> " this is " <> cid)
      , simpleTestCase "call_data_append really appends" $ \cid -> do
        do call cid $
            callNew (bytes cid) (bytes "query")
                    (callback replyArgData) (callback replyRejectData) >>>
            callDataAppend (bytes (BS.take 3 (run defaultOtherSide))) >>>
            callDataAppend (bytes (BS.drop 3 (run defaultOtherSide))) >>>
            callPerform
         >>= is ("Hello " <> cid <> " this is " <> cid)
      ]

    , simpleTestCase "to nonexistant canister" $ \cid ->
      call' cid (inter_call "foo" "bar" defArgs) >>= isRelayReject [3]

    , simpleTestCase "to nonexistant method" $ \cid ->
      call' cid (inter_call cid "bar" defArgs) >>= isRelayReject [3]

    , simpleTestCase "Call from query method traps (in update call)" $ \cid ->
      callToQuery' cid (inter_query cid defArgs) >>= isReject [5]

    , simpleTestCase "Call from query method traps (in query call)" $ \cid ->
      query' cid (inter_query cid defArgs) >>= isReject [5]

    , simpleTestCase "Call from query method traps (in inter-canister-call)" $ \cid ->
      do call' cid $
          inter_call cid "query" defArgs {
            other_side = inter_query cid defArgs
          }
        >>= isRelayReject [5]

    , simpleTestCase "Self-call (to update)" $ \cid ->
      call cid (inter_update cid defArgs)
        >>= is ("Hello " <> cid <> " this is " <> cid)

    , simpleTestCase "Self-call (to query)" $ \cid -> do
      call cid (inter_query cid defArgs)
        >>= is ("Hello " <> cid <> " this is " <> cid)

    , simpleTestCase "update commits" $ \cid -> do
      call_ cid $
        setGlobal "FOO" >>>
        inter_update cid defArgs{ other_side = setGlobal "BAR" >>> reply }

      query cid (replyData getGlobal) >>= is "BAR"

    , simpleTestCase "query does not commit" $ \cid -> do
      call_ cid $
        setGlobal "FOO" >>>
        inter_query cid defArgs{ other_side = setGlobal "BAR" >>> reply }

      do query cid $ replyData getGlobal
        >>= is "FOO"

    , simpleTestCase "query no response" $ \cid ->
      do call' cid $ inter_query cid defArgs{ other_side = noop }
        >>= isRelayReject [5]

    , simpleTestCase "query double reply" $ \cid ->
      do call' cid $ inter_query cid defArgs{ other_side = reply >>> reply }
        >>= isRelayReject [5]

    , simpleTestCase "Reject code is 0 in reply" $ \cid ->
      do call' cid $ inter_query cid defArgs{ on_reply = replyData (i2b reject_code) }
        >>= isRelayReject [0]

    , simpleTestCase "Second reply in callback" $ \cid -> do
      do call cid $
          setGlobal "FOO" >>>
          replyData "First reply" >>>
          inter_query cid defArgs{
            on_reply = setGlobal "BAR" >>> replyData "Second reply",
            on_reject = setGlobal "BAZ" >>> replyRejectData
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
        >>= is ("Hello " <> cid <> " this is " <> cid2)

    , simpleTestCase "Call to other canister (to query)" $ \cid -> do
      cid2 <- install noop
      do call cid $ inter_query cid2 defArgs
        >>= is ("Hello " <> cid <> " this is " <> cid2)
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

    step "Writing stable memory"
    call_ cid $ stableWrite (int 0) "FOO" >>> reply

    step "Writing stable memory (query)"
    query_ cid $ stableWrite (int 0) "BAR" >>> reply

    step "Reading stable memory"
    call cid (replyData (stableRead (int 0) (int 3))) >>= is "FOO"

  , testGroup "time" $
    let getTimeTwice = cat (i64tob getTime) (i64tob getTime) in
    [ simpleTestCase "in query" $ \cid ->
      query cid (replyData getTimeTwice) >>= as2Word64 >>= bothSame
    , simpleTestCase "in update" $ \cid ->
      query cid (replyData getTimeTwice) >>= as2Word64 >>= bothSame
    , testCase "in install" $ do
      cid <- install $ setGlobal (getTimeTwice)
      query cid (replyData getGlobal) >>= as2Word64 >>= bothSame
    , testCase "in pre_upgrade" $ do
      cid <- install $
        ignore (stableGrow (int 1)) >>>
        onPreUpgrade (callback $ stableWrite (int 0) (getTimeTwice))
      upgrade cid noop
      query cid (replyData (stableRead (int 0) (int (2*8)))) >>= as2Word64 >>= bothSame
    , simpleTestCase "in post_upgrade" $ \cid -> do
      upgrade cid $ setGlobal (getTimeTwice)
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

  , testGroup "reinstall"
    [ testCase "succeeding" $ do
      cid <- install $
            setGlobal "FOO" >>>
            ignore (stableGrow (int 1)) >>>
            stableWrite (int 0) "FOO______"
      query cid (replyData getGlobal) >>= is "FOO"
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "FOO______"
      query cid (replyData (i2b stableSize)) >>= is "\1\0\0\0"

      reinstall cid $
        setGlobal "BAR" >>>
        ignore (stableGrow (int 2)) >>>
        stableWrite (int 0) "BAR______"

      query cid (replyData getGlobal) >>= is "BAR"
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "BAR______"
      query cid (replyData (i2b stableSize)) >>= is "\2\0\0\0"

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
        cid <- ic_create ic00
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
        omitFields queryToNonExistant $ \req -> do
          addExpiry req >>= envelope defaultSK >>= postCBOR "/api/v1/read" >>= code4xx

    , testCase "non-existing canister" $ do
        addExpiry queryToNonExistant
          >>= envelopeFor anonymousUser
          >>= postCBOR "/api/v1/read"
          >>= okCBOR >>= queryResponse >>= isReject [3]
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
          submitCBOR req >>= isReply >>= is "\xff\xff"

          -- check that the request is there
          getRequestStatus user (requestId req) >>= is (Responded (Reply "\xff\xff"))

          return (requestId req)
    in
    [ testGroup "required fields" $
        omitFields readStateEmpty $ \req ->
          addExpiry req >>= envelope defaultSK >>= postCBOR "/api/v1/read" >>= code4xx

    , testCase "certificate validates" $ do
        cert <- getStateCert defaultUser []
        validateStateCert cert

    , testCase "time is present" $ do
        cert <- getStateCert defaultUser []
        void $ certValue @Natural cert ["time"]

    , testCase "time can be asked for" $ do
        cert <- getStateCert defaultUser [["time"]]
        void $ certValue @Natural cert ["time"]

    , testGroup "non-existence proofs for non-existing request id"
        [ testCase ("rid \"" ++ shorten 8 (asHex rid) ++ "\"") $ do
            cert <- getStateCert defaultUser [["request_status", rid]]
            certValueAbsent cert ["request_status", rid, "status"]
        | rid <- [ "", BS.replicate 32 0, BS.replicate 32 8, BS.replicate 32 255 ]
        ]

    , simpleTestCase "can ask for portion of request status " $ \cid -> do
        rid <- ensure_request_exists cid defaultUser
        cert <- getStateCert defaultUser
          [["request_status", rid, "status"], ["request_status", rid, "reply"]]
        void $ certValue @T.Text cert ["request_status", rid, "status"]
        void $ certValue @Blob cert ["request_status", rid, "reply"]

    , simpleTestCase "access denied for other users request" $ \cid -> do
        rid <- ensure_request_exists cid defaultUser
        getStateCert' otherUser [["request_status", rid]] >>= code4xx

    , simpleTestCase "reading two statuses in one go" $ \cid -> do
        rid1 <- ensure_request_exists cid defaultUser
        rid2 <- ensure_request_exists cid defaultUser
        cert <- getStateCert defaultUser [["request_status", rid1], ["request_status", rid2]]
        void $ certValue @T.Text cert ["request_status", rid1, "status"]
        void $ certValue @T.Text cert ["request_status", rid2, "status"]

    , simpleTestCase "access denied for other users request (mixed request)" $ \cid -> do
        rid1 <- ensure_request_exists cid defaultUser
        rid2 <- ensure_request_exists cid otherUser
        getStateCert' defaultUser [["request_status", rid1], ["request_status", rid2]] >>= code4xx

    , testCase "access denied for bogus path" $ do
        getStateCert' otherUser [["hello", "world"]] >>= code4xx

    , testCase "access denied for fetching full state tree" $ do
        getStateCert' otherUser [[]] >>= code4xx
    ]

  , testGroup "certified variables" $
    let extract :: Blob -> Blob -> IO Blob
        extract cid b = do
          cert <- decodeStateCert b
          case wellFormed (cert_tree cert) of
              Left err -> assertFailure $ "Hash tree not well formed: " ++ err
              Right () -> return ()
          certValue cert ["canister", cid, "certified_data"]
    in
    [ simpleTestCase "initially empty" $ \cid -> do
      query cid (replyData getCertificate) >>= extract cid >>= is ""
    , simpleTestCase "validates" $ \cid -> do
      query cid (replyData getCertificate)
        >>= decodeStateCert >>= validateStateCert
    , simpleTestCase "present in query method (query call)" $ \cid -> do
      query cid (replyData (i2b getCertificatePresent))
        >>= is "\1\0\0\0"
    , simpleTestCase "not present in query method (update call)" $ \cid -> do
      callToQuery' cid (replyData (i2b getCertificatePresent))
        >>= isReply >>= is "\0\0\0\0"
    , simpleTestCase "not present in query method (inter-canister call)" $ \cid -> do
      do call cid $
          inter_call cid "query" defArgs {
            other_side = replyData (i2b getCertificatePresent)
          }
        >>= is "\0\0\0\0"
    , simpleTestCase "not present in update method" $ \cid -> do
      call cid (replyData (i2b getCertificatePresent))
        >>= is "\0\0\0\0"

    , simpleTestCase "set and get" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> reply
      query cid (replyData getCertificate) >>= extract cid >>= is "FOO"
    , simpleTestCase "set twice" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> setCertifiedData "BAR" >>> reply
      query cid (replyData getCertificate) >>= extract cid >>= is "BAR"
    , simpleTestCase "set then trap" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> reply
      call' cid (setCertifiedData "BAR" >>> trap "Trapped") >>= isReject [5]
      query cid (replyData getCertificate) >>= extract cid >>= is "FOO"
    , simpleTestCase "too large traps, old value retained" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> reply
      call' cid (setCertifiedData (bytes (BS.replicate 33 0x42)) >>> reply)
        >>= isReject [5]
      query cid (replyData getCertificate) >>= extract cid >>= is "FOO"
    , testCase "set in init" $ do
      cid <- install $ setCertifiedData "FOO"
      query cid (replyData getCertificate) >>= extract cid >>= is "FOO"
    , testCase "set in pre-upgrade" $ do
      cid <- install $ onPreUpgrade (callback $ setCertifiedData "FOO")
      upgrade cid noop
      query cid (replyData getCertificate) >>= extract cid >>= is "FOO"
    , simpleTestCase "set in post-upgrade" $ \cid -> do
      upgrade cid $ setCertifiedData "FOO"
      query cid (replyData getCertificate) >>= extract cid >>= is "FOO"
    ]

  , testGroup "cycles" $
    let replyBalance = replyData (i64tob getBalance)
        rememberBalance =
          ignore (stableGrow (int 1)) >>>
          stableWrite (int 0) (i64tob getBalance)
        recallBalance = replyData (stableRead (int 0) (int 8))
        acceptAll = ignore (acceptCycles getAvailableCycles)
        queryBalance cid = query cid replyBalance >>= asWord64

        def_cycles = 1000_000_000_000 :: Word64
        -- we assume that cycle balances do not change more than one million
        -- while running these tests.
        eps = 20_000_000_000 :: Integer

        isRoughly :: (HasCallStack, Show a, Num a, Integral a) => a -> a -> Assertion
        isRoughly exp act = assertBool
           (show act ++ " not near " ++ show exp)
           (abs (fromIntegral exp - fromIntegral act) < eps)

        create prog = do
          cid <- ic_create_with_cycles ic00 (Just (fromIntegral def_cycles))
          installAt cid prog
          return cid
        create_via cid initial_cycles = do
          cid2 <- ic_create (ic00viaWithCycles cid initial_cycles)
          universal_wasm <- getTestWasm "universal_canister"
          ic_install (ic00via cid) (enum #install) cid2 universal_wasm (run noop)
          return cid2
    in
    [ testGroup "can use balance API" $
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
    , simpleTestCase "cant accept absurd amount of cycles" $ \cid ->
        call cid (replyData (i64tob (acceptCycles (int64 maxBound)))) >>= asWord64 >>= is 0

    , testGroup "provisional_create_canister_with_cycles"
      [ testCase "balance as expected" $ do
        cid <- create noop
        queryBalance cid >>= isRoughly def_cycles

      , testCaseSteps "default (i.e. max) balance" $ \step -> do
        cid <- ic_create_with_cycles ic00 Nothing
        installAt cid noop
        cycles <- queryBalance cid
        step $ "Cycle balance now at " ++ show cycles

      , testCaseSteps "> 2^128 succeeds" $ \step -> do
        cid <- ic_create_with_cycles ic00 (Just (10 * 2^(128::Int)))
        installAt cid noop
        cycles <- queryBalance cid
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
        >>= asWord64 >>= isRoughly (def_cycles `div` 4)
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
        >>= asWord64 >>= isRoughly (def_cycles - def_cycles `div` 4)
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
      [ testCase "deposit cycles (as controller)" $ do
        cid1 <- create noop
        cid2 <- create_via cid1 (def_cycles`div`2)
        queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
        queryBalance cid2 >>= isRoughly (def_cycles `div` 2)
        ic_deposit_cycles (ic00viaWithCycles cid1 (def_cycles`div`4)) cid2
        queryBalance cid1 >>= isRoughly (def_cycles `div` 4)
        queryBalance cid2 >>= isRoughly (def_cycles - def_cycles `div` 4)
      , testCase "deposit cycles (as controller, too much)" $ do
        cid1 <- create noop
        cid2 <- create_via cid1 (def_cycles`div`2)
        queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
        queryBalance cid2 >>= isRoughly (def_cycles `div` 2)
        ic_deposit_cycles' (ic00viaWithCycles cid1 def_cycles) cid2 >>= isReject [4,5]
        queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
        queryBalance cid2 >>= isRoughly (def_cycles `div` 2)
      , testCase "deposit cycles (as wrong controller)" $ do
        cid1 <- create noop
        cid2 <- create_via cid1 (def_cycles`div`2)
        queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
        queryBalance cid2 >>= isRoughly (def_cycles `div` 2)
        ic_deposit_cycles' (ic00viaWithCycles cid2 (def_cycles`div`4)) cid1 >>= isRelayReject [4,5]
        queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
        queryBalance cid2 >>= isRoughly (def_cycles `div` 2)
      , testCase "deposit cycles (as user controller, zero cycles)" $ do
        cid1 <- create noop
        queryBalance cid1 >>= isRoughly def_cycles
        ic_deposit_cycles ic00 cid1
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
        cycles <- queryBalance cid
        step $ "Cycle balance now at " ++ show cycles
      , testCase "nonexisting canister" $ do
        ic_top_up' ic00 doesn'tExist (fromIntegral def_cycles)
          >>= isReject [3,5]
      ]
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

      env dels req =
        delegationEnv defaultSK
          (zip [createSecretKeyEd25519 (BS.singleton n) | n <- [0..]] dels) req

      good req dels = do
        req <- addExpiry req
        let rid = requestId req
        -- sign request with delegations
        env dels req >>= postCBOR "/api/v1/submit" >>= code2xx
        -- wait for it
        void $ awaitStatus defaultUser rid >>= isReply
        -- also read status with delegation
        sreq <- addExpiry $ rec
          [ "request_type" =: GText "read_state"
          , "sender" =: GBlob defaultUser
          , "paths" =: GList [GList [GBlob "request_status", GBlob rid]]
          ]
        env dels sreq >>= postCBOR "/api/v1/read" >>= void . code2xx

      badSubmit req dels = do
        req <- addExpiry req
        -- sign request with delegations (should fail)
        env dels req >>= postCBOR "/api/v1/submit" >>= code4xx

      badRead req dels = do
        req <- addExpiry req
        let rid = requestId req
        -- submit with plain signature
        envelope defaultSK req >>= postCBOR "/api/v1/submit" >>= code202
        -- wait for it
        void $ awaitStatus defaultUser rid >>= isReply
        -- also read status with delegation
        sreq <- addExpiry $ rec
          [ "request_type" =: GText "read_state"
          , "sender" =: GBlob defaultUser
          , "paths" =: GList [GList [GBlob "request_status", GBlob rid]]
          ]
        env dels sreq >>= postCBOR "/api/v1/read" >>= void . code4xx

      goodTestCase name mkReq mkDels =
        simpleTestCase name $ \cid -> good (mkReq cid) (mkDels cid)

      badTestCase name mkReq mkDels = testGroup name
        [ simpleTestCase "in submit" $ \cid -> badSubmit (mkReq cid) (mkDels cid)
        , simpleTestCase "in read_state" $ \cid -> badRead (mkReq cid) (mkDels cid)
        ]

    in
    [ goodTestCase "one delegation, singleton target" callReq $ \cid ->
      [Just [cid]]
    , badTestCase "one delegation, wrong singleton target" callReq $ \_cid ->
      [Just [doesn'tExist]]
    , goodTestCase "one delegation, two targets" callReq $ \cid ->
      [Just [cid, doesn'tExist]]
    , goodTestCase "one delegation, redundant targets" callReq $ \cid ->
      [Just [cid, cid, doesn'tExist]]
    , goodTestCase "two delegations, singletons" callReq $ \cid ->
      [Just [cid], Just [cid] ]
    , goodTestCase "two delegations, first restricted" callReq $ \cid ->
      [Just [cid], Nothing ]
    , goodTestCase "two delegations, second restricted" callReq $ \cid ->
      [Nothing, Just [cid]]
    , badTestCase "two delegations, empty intersection" callReq $ \cid ->
      [Just [cid], Just [doesn'tExist]]
    , badTestCase "two delegations, first empty target set" callReq $ \cid ->
      [Just [], Just [cid]]
    , badTestCase "two delegations, second empty target set" callReq $ \cid ->
      [Just [cid], Just []]
    , goodTestCase "management canister: correct target" mgmtReq $ \_cid ->
      [Just [""]]
    , badTestCase "management canister: empty target set" mgmtReq $ \_cid ->
      [Just []]
    , badTestCase "management canister: bogus target" mgmtReq $ \_cid ->
      [Just [doesn'tExist]]
    , badTestCase "management canister: bogus target (using target canister)" mgmtReq $ \cid ->
      [Just [cid]]
    ]

  , testGroup "Authentication schemes" $
    let ed25519SK2 = createSecretKeyEd25519 "more keys"
        ed25519SK3 = createSecretKeyEd25519 "yet more keys"
        ed25519SK4 = createSecretKeyEd25519 "even more keys"
        delEnv sks = delegationEnv ed25519SK (map (, Nothing) sks) -- no targets in these tests
    in flip foldMap
      [ ("Ed25519",            ed25519User,  envelope ed25519SK)
      , ("Ed25519 (raw)",      defaultUser,  envelope defaultSK)
      , ("WebAuthn",           webAuthnUser, envelope webAuthnSK)
      , ("empty delegations",  ed25519User,  delEnv [])
      , ("same delegations",   ed25519User,  delEnv [ed25519SK])
      , ("three delegations",  ed25519User,  delEnv [ed25519SK2, ed25519SK3])
      , ("four delegations",   ed25519User,  delEnv [ed25519SK2, ed25519SK3, ed25519SK4])
      , ("mixed delegations",  ed25519User,  delEnv [defaultSK, webAuthnSK])
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
      postCBOR "/api/v1/read" signed_req >>= okCBOR >>= queryResponse >>= isReply >>= is ""

    , simpleTestCase (name ++ " in update") $ \cid -> do
      req <- addExpiry $ rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob user
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "update"
            , "arg" =: GBlob (run reply)
            ]
      signed_req <- env req
      postCBOR "/api/v1/submit" signed_req >>= code2xx

      awaitStatus user (requestId req) >>= isReply >>= is ""
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
        readCBOR good_req >>= queryResponse >>= isReply >>= is ""
        env (mod_req good_req) >>= postCBOR "/api/v1/read" >>= code4xx

      , testCase "in empty read state request" $ do
          good_req <- addNonce >=> addExpiry $ readStateEmpty
          envelope defaultSK good_req >>= postCBOR "/api/v1/read" >>= code2xx
          env (mod_req good_req) >>= postCBOR "/api/v1/read"  >>= code4xx

      , simpleTestCase "in call" $ \cid -> do
          good_req <- addNonce >=> addExpiry $ rec
                [ "request_type" =: GText "call"
                , "sender" =: GBlob defaultUser
                , "canister_id" =: GBlob cid
                , "method_name" =: GText "query"
                , "arg" =: GBlob (run reply)
                ]
          let req = mod_req good_req
          env req >>= postCBOR "/api/v1/submit" >>= code202_or_4xx

          -- Also check that the request was not created
          ingressDelay
          getRequestStatus defaultUser (requestId req) >>= is UnknownStatus

          -- check that with a valid signature, this would have worked
          submitCBOR good_req >>= isReply >>= is ""
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

-- | Posting a CBOR request, returning a raw bytestring
postCBOR :: (HasCallStack, HasTestConfig) => String -> GenR -> IO (Response BS.ByteString)
postCBOR path gr = do
    request <- parseRequest $ endPoint ++ path
    request <- return $ request
      { method = "POST"
      , requestBody = RequestBodyLBS $ BS.toLazyByteString $ encode gr
      , requestHeaders = [(hContentType, "application/cbor")]
      }
    httpLbs request testManager

-- | Add envelope to CBOR request, add a nonce and expiry if it is not there,
-- post to "read", return decoded CBOR
readCBOR :: (HasCallStack, HasTestConfig) => GenR -> IO GenR
readCBOR req = do
  req <- addNonce req
  req <- addExpiry req
  envelopeFor (senderOf req) req >>= postCBOR "/api/v1/read" >>= okCBOR

-- | Add envelope to CBOR, and a nonce and expiry if not there, post to
-- "submit", poll for the request response, and return decoded CBOR
submitCBOR :: (HasCallStack, HasTestConfig) => GenR -> IO ReqResponse
submitCBOR req = do
  req <- addNonce req
  req <- addExpiry req
  res <- envelopeFor (senderOf req) req >>= postCBOR "/api/v1/submit"
  code202 res
  assertBool "Response body not empty" (BS.null (responseBody res))
  awaitStatus (senderOf req) (requestId req)

-- | Submits twice
submitCBORTwice :: HasTestConfig => GenR -> IO ReqResponse
submitCBORTwice req = do
  req <- addNonce req
  req <- addExpiry req
  res <- envelopeFor (senderOf req) req >>= postCBOR "/api/v1/submit"
  code202 res
  res <- envelopeFor (senderOf req) req >>= postCBOR "/api/v1/submit"
  code202 res
  assertBool "Response body not empty" (BS.null (responseBody res))
  awaitStatus (senderOf req) (requestId req)

getStateCert' :: (HasCallStack, HasTestConfig) => Blob -> [[Blob]] -> IO (Response Blob)
getStateCert' sender paths = do
    req <- addExpiry $ rec
      [ "request_type" =: GText "read_state"
      , "sender" =: GBlob sender
      , "paths" =: GList (map (GList . map GBlob) paths)
      ]
    envelopeFor (senderOf req) req >>= postCBOR "/api/v1/read"

decodeStateCert :: HasCallStack => Blob -> IO Certificate
decodeStateCert b = either assertFailure return $ decodeCert b

getStateCert :: (HasCallStack, HasTestConfig) => Blob -> [[Blob]] -> IO Certificate
getStateCert sender paths = do
    gr <- getStateCert' sender paths >>= okCBOR
    b <- record (field blob "certificate") gr
    cert <- decodeStateCert b

    case wellFormed (cert_tree cert) of
        Left err -> assertFailure $ "Hash tree not well formed: " ++ err
        Right () -> return ()

    return cert

verboseVerify :: Blob -> Blob -> Blob -> Blob -> IO ()
verboseVerify domain_sep pk msg sig =
    case verify domain_sep pk msg sig of
        Left err -> assertFailure $ unlines
            [ "Signature verification failed"
            , T.unpack err
            , "Domain separator:   " ++ prettyBlob domain_sep
            , "Public key (DER):   " ++ asHex pk
            , "Public key decoded: " ++
               case DER.decode pk of
                 Left err -> err
                 Right (suite, key) -> asHex key ++ " (" ++ show suite ++ ")"
            , "Signature:          " ++ asHex sig
            , "Checked message:    " ++ prettyBlob msg
            ]
        Right () -> return ()

validateDelegation :: (HasCallStack, HasTestConfig) => Maybe Delegation -> IO Blob
validateDelegation Nothing = return (tc_root_key testConfig)
validateDelegation (Just del) = do
    cert <- either assertFailure return $ decodeCert (del_certificate del)
    case wellFormed (cert_tree cert) of
        Left err -> assertFailure $ "Hash tree not well formed: " ++ err
        Right () -> return ()
    validateStateCert cert

    certValue cert ["subnet", del_subnet_id del, "public_key"]

validateStateCert :: (HasCallStack, HasTestConfig) => Certificate -> IO ()
validateStateCert cert = do
    pk <- validateDelegation (cert_delegation cert)
    verboseVerify "ic-state-root" pk (reconstruct (cert_tree cert)) (cert_sig cert)

data ReqResponse = Reply Blob | Reject Natural T.Text
  deriving (Eq, Show)
data ReqStatus = Processing | Pending | Responded ReqResponse | UnknownStatus
  deriving (Eq, Show)

prettyPath :: [Blob] -> String
prettyPath = concatMap (("/" ++) . shorten 15 . prettyBlob)

prettyBlob :: Blob -> String
prettyBlob x =
  let s = map (chr . fromIntegral) (BS.unpack x) in
  if all isPrint s then s else asHex x

certValue :: HasCallStack => CertVal a => Certificate -> [Blob] -> IO a
certValue cert path = case lookupPath (cert_tree cert) path of
    Found b -> case fromCertVal b of
      Just x -> return x
      Nothing -> assertFailure $ "Cannot parse " ++ prettyPath path ++ " from " ++ show b
    x -> assertFailure $ "Expected to find " ++ prettyPath path ++ ", but got " ++ show x

certValueAbsent :: HasCallStack => Certificate -> [Blob] -> IO ()
certValueAbsent cert path = case lookupPath (cert_tree cert) path of
    Absent -> return ()
    x -> assertFailure $ "Path " ++ prettyPath path ++ " should be absent, but got " ++ show x

getRequestStatus :: (HasCallStack, HasTestConfig) => Blob -> Blob -> IO ReqStatus
getRequestStatus sender rid = do
    cert <- getStateCert sender [["request_status", rid]]

    case lookupPath (cert_tree cert) ["request_status", rid, "status"] of
      Absent -> return UnknownStatus
      Found "processing" -> return Processing
      Found "received" -> return Pending
      Found "replied" -> do
        b <- certValue cert ["request_status", rid, "reply"]
        certValueAbsent cert ["request_status", rid, "reject_code"]
        certValueAbsent cert ["request_status", rid, "reject_message"]
        return $ Responded (Reply b)
      Found "rejected" -> do
        certValueAbsent cert ["request_status", rid, "reply"]
        code <- certValue cert ["request_status", rid, "reject_code"]
        msg <- certValue cert ["request_status", rid, "reject_message"]
        return $ Responded (Reject code msg)
      Found s -> assertFailure $ "Unexpected status " ++ show s
      -- This case should not happen with a compliant IC, but let
      -- us be liberal here, and strict in a dedicated test
      Unknown -> return UnknownStatus
      x -> assertFailure $ "Unexpected request status, got " ++ show x

awaitStatus :: HasTestConfig => Blob -> Blob -> IO ReqResponse
awaitStatus sender rid = loop $ pollDelay >> getRequestStatus sender rid >>= \case
    Responded x -> return $ Just x
    _ -> return Nothing
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

queryResponse :: GenR -> IO ReqResponse
queryResponse = record $ do
    s <- field text "status"
    case s of
      "replied" ->
        Reply <$> field (record (field blob "arg")) "reply"
      "rejected" -> do
        code <- field nat "reject_code"
        msg <- field text "reject_message"
        return $ Reject code msg
      _ -> lift $ assertFailure $ "Unexpected status " ++ show s

isReject :: HasCallStack => [Natural] -> ReqResponse -> IO ()
isReject _ (Reply _) = assertFailure "Expected reject, got reply"
isReject codes (Reject n msg) = do
  assertBool
    ("Reject code " ++ show n ++ " not in " ++ show codes ++ "\n" ++ T.unpack msg)
    (n `elem` codes)

isReply :: HasCallStack => ReqResponse -> IO Blob
isReply (Reply b) = return b
isReply (Reject n msg) =
  assertFailure $ "Unexpected reject (code " ++ show n ++ "): " ++ T.unpack msg

-- Convenience decoders
asWord32 :: HasCallStack => Blob -> IO Word32
asWord32 = runGet Get.getWord32le

asWord64 :: HasCallStack => Blob -> IO Word64
asWord64 = runGet Get.getWord64le

as2Word64 :: HasCallStack => Blob -> IO (Word64, Word64)
as2Word64 = runGet $ (,) <$> Get.getWord64le <*> Get.getWord64le

bothSame :: (Eq a, Show a) => (a, a) -> Assertion
bothSame (x,y) = x @?= y

runGet :: HasCallStack => Get.Get a -> Blob -> IO a
runGet a b = case  Get.runGetOrFail (a <* done) b of
    Left (_,_, err) ->
        fail $ "Could not parse " ++ show b ++ ": " ++ err
    Right (_,_, x) -> return x
  where
    done = do
        nothing_left <- Get.isEmpty
        unless nothing_left (fail "left-over bytes")

is :: (HasCallStack, Eq a, Show a) => a -> a -> Assertion
is exp act = act @?= exp

-- A reject forwarded by replyRejectData
isRelayReject :: HasCallStack => [Word32] -> ReqResponse -> IO ()
isRelayReject codes r = do
  b <- isReply r
  assertBool
    ("Reject code " ++ show b ++ " not in " ++ show codes ++ "\n")
    (BS.take 4 b `elem` map (BS.toLazyByteString . BS.word32LE) codes)

data StatusResponse = StatusResponse
    { status_api_version :: T.Text
    , status_root_key :: Blob
    }

statusResonse :: HasCallStack => GenR -> IO StatusResponse
statusResonse = record $ do
    v <- field text "ic_api_version"
    _ <- optionalField text "impl_source"
    _ <- optionalField text "impl_version"
    _ <- optionalField text "impl_revision"
    pk <- field blob "root_key"
    swallowAllFields -- More fields are explicitly allowed
    return StatusResponse {status_api_version = v, status_root_key = pk }

-- * Interacting with aaaaa-aa (via HTTP)

-- how to reach the management canister
type IC00 = T.Text -> Blob -> IO ReqResponse

ic00 :: HasTestConfig => IC00
ic00 method_name arg = submitCBOR $ rec
      [ "request_type" =: GText "call"
      , "sender" =: GBlob defaultUser
      , "canister_id" =: GBlob ""
      , "method_name" =: GText method_name
      , "arg" =: GBlob arg
      ]

ic00as :: HasTestConfig => Blob -> IC00
ic00as user method_name arg = submitCBOR $ rec
      [ "request_type" =: GText "call"
      , "sender" =: GBlob user
      , "canister_id" =: GBlob ""
      , "method_name" =: GText method_name
      , "arg" =: GBlob arg
      ]

ic00via :: HasTestConfig => Blob -> IC00
ic00via cid = ic00viaWithCycles cid 0

ic00viaWithCycles :: HasTestConfig => Blob -> Word64 -> IC00
ic00viaWithCycles cid cycles method_name arg =
  call' cid $
    callNew
      (bytes "") (bytes (BS.fromStrict (T.encodeUtf8 method_name))) -- aaaaa-aa
      (callback replyArgData) (callback replyRejectData) >>>
    callDataAppend (bytes arg) >>>
    callCyclesAdd (int64 cycles) >>>
    callPerform

managementService :: (HasCallStack, HasTestConfig) => IC00 -> Rec (ICManagement IO)
managementService ic00 =
  Candid.toCandidService err $ \method_name arg ->
    ic00 method_name arg >>= isReply
  where
    err s = assertFailure $ "Candid decoding error: " ++ s

ic_create :: HasTestConfig => IC00 -> IO Blob
ic_create ic00 = do
  r <- managementService ic00 .! #create_canister $ ()
  return (rawPrincipal (r .! #canister_id))

ic_create_with_cycles :: HasTestConfig => IC00 -> Maybe Natural -> IO Blob
ic_create_with_cycles ic00 cycles = do
  r <- managementService ic00 .! #provisional_create_canister_with_cycles $ empty
    .+ #amount .== cycles
  return (rawPrincipal (r .! #canister_id))

ic_install :: HasTestConfig => IC00 -> InstallMode -> Blob -> Blob -> Blob -> IO ()
ic_install ic00 mode canister_id wasm_module arg = do
  managementService ic00 .! #install_code $ empty
    .+ #mode .== mode
    .+ #canister_id .== Principal canister_id
    .+ #wasm_module .== wasm_module
    .+ #arg .== arg
    .+ #compute_allocation .== Nothing
    .+ #memory_allocation .== Just (100 * 1024 * 1024)

ic_set_controller :: HasTestConfig => IC00 -> Blob -> Blob -> IO ()
ic_set_controller ic00 canister_id new_controller = do
  managementService ic00 .! #set_controller $ empty
    .+ #canister_id .== Principal canister_id
    .+ #new_controller .== Principal new_controller

ic_start_canister :: HasTestConfig => IC00 -> Blob -> IO ()
ic_start_canister ic00 canister_id = do
  managementService ic00 .! #start_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_stop_canister :: HasTestConfig => IC00 -> Blob -> IO ()
ic_stop_canister ic00 canister_id = do
  managementService ic00 .! #stop_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_canister_status ::
    forall a b. (a -> IO b) ~ (ICManagement IO .! "canister_status") =>
    HasTestConfig => IC00 -> Blob -> IO b
ic_canister_status ic00 canister_id = do
  managementService ic00 .! #canister_status $ empty
    .+ #canister_id .== Principal canister_id

ic_deposit_cycles :: HasTestConfig => IC00 -> Blob -> IO ()
ic_deposit_cycles ic00 canister_id = do
  managementService ic00 .! #deposit_cycles $ empty
    .+ #canister_id .== Principal canister_id

ic_top_up :: HasTestConfig => IC00 -> Blob -> Natural -> IO ()
ic_top_up ic00 canister_id amount = do
  managementService ic00 .! #provisional_top_up_canister $ empty
    .+ #canister_id .== Principal canister_id
    .+ #amount .== amount

ic_delete_canister :: HasTestConfig => IC00 -> Blob -> IO ()
ic_delete_canister ic00 canister_id = do
  managementService ic00 .! #delete_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_raw_rand :: HasTestConfig => IC00 -> IO Blob
ic_raw_rand ic00 = managementService ic00 .! #raw_rand $ ()

-- Primed variants return the request
callIC' :: forall s a b.
  HasTestConfig =>
  KnownSymbol s =>
  (a -> IO b) ~ (ICManagement IO .! s) =>
  Candid.CandidArg a =>
  IC00 -> Label s -> a -> IO ReqResponse
callIC' ic00 l x = ic00 (T.pack (symbolVal l)) (Candid.encode x)

ic_install' :: HasTestConfig => IC00 -> InstallMode -> Blob -> Blob -> Blob -> IO ReqResponse
ic_install' ic00 mode canister_id wasm_module arg =
  callIC' ic00 #install_code $ empty
    .+ #mode .== mode
    .+ #canister_id .== Principal canister_id
    .+ #wasm_module .== wasm_module
    .+ #arg .== arg
    .+ #compute_allocation .== Nothing
    .+ #memory_allocation .== Just (100 * 1024 * 1024)

ic_set_controller' :: HasTestConfig => IC00 -> Blob -> Blob -> IO ReqResponse
ic_set_controller' ic00 canister_id new_controller = do
  callIC' ic00 #set_controller $ empty
    .+ #canister_id .== Principal canister_id
    .+ #new_controller .== Principal new_controller

ic_start_canister' :: HasTestConfig => IC00 -> Blob -> IO ReqResponse
ic_start_canister' ic00 canister_id = do
  callIC' ic00 #start_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_stop_canister' :: HasTestConfig => IC00 -> Blob -> IO ReqResponse
ic_stop_canister' ic00 canister_id = do
  callIC' ic00 #stop_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_canister_status' :: HasTestConfig => IC00 -> Blob -> IO ReqResponse
ic_canister_status' ic00 canister_id = do
  callIC' ic00 #canister_status $ empty
    .+ #canister_id .== Principal canister_id

ic_delete_canister' :: HasTestConfig => IC00 -> Blob -> IO ReqResponse
ic_delete_canister' ic00 canister_id = do
  callIC' ic00 #delete_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_deposit_cycles' :: HasTestConfig => IC00 -> Blob -> IO ReqResponse
ic_deposit_cycles' ic00 canister_id = do
  callIC' ic00 #deposit_cycles $ empty
    .+ #canister_id .== Principal canister_id

ic_top_up' :: HasTestConfig => IC00 -> Blob -> Natural -> IO ReqResponse
ic_top_up' ic00 canister_id amount = do
  callIC' ic00 #provisional_top_up_canister $ empty
    .+ #canister_id .== Principal canister_id
    .+ #amount .== amount

-- A barrier

-- This will stop and start all mentioned canisters. This guarantees
-- that all outstanding callbacks are handled
barrier :: HasTestConfig => [Blob] -> IO ()
barrier cids = do
  mapM_ (ic_stop_canister ic00) cids
  mapM_ (ic_start_canister ic00) cids

-- * Interacting with the universal canister

-- Some common operations on the universal canister
-- The primed variant (call') return the response record,
-- e.g. to check for error conditions.
-- The unprimed variant expect a reply.

install' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
install' cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install' ic00 (enum #install) cid universal_wasm (run prog)

installAt :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ()
installAt cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install ic00 (enum #install) cid universal_wasm (run prog)

-- Also calls create, used default 'ic00'
install :: (HasCallStack, HasTestConfig) => Prog -> IO Blob
install prog = do
    cid <- ic_create ic00
    installAt cid prog
    return cid

upgrade' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
upgrade' cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install' ic00 (enum #upgrade) cid universal_wasm (run prog)

upgrade :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ()
upgrade cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install ic00 (enum #upgrade) cid universal_wasm (run prog)

reinstall' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
reinstall' cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install' ic00 (enum #reinstall) cid universal_wasm (run prog)

reinstall :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ()
reinstall cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install ic00 (enum #reinstall) cid universal_wasm (run prog)

callRequest :: HasTestConfig => Blob -> Prog -> GenR
callRequest cid prog = rec
    [ "request_type" =: GText "call"
    , "sender" =: GBlob defaultUser
    , "canister_id" =: GBlob cid
    , "method_name" =: GText "update"
    , "arg" =: GBlob (run prog)
    ]

callToQuery' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
callToQuery' cid prog = submitCBOR $ rec
    [ "request_type" =: GText "call"
    , "sender" =: GBlob defaultUser
    , "canister_id" =: GBlob cid
    , "method_name" =: GText "query"
    , "arg" =: GBlob (run prog)
    ]

call' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
call' cid prog = submitCBOR (callRequest cid prog)

callTwice' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
callTwice' cid prog = submitCBORTwice (callRequest cid prog)

call :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO Blob
call cid prog = call' cid prog >>= isReply

call_ :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ()
call_ cid prog = call cid prog >>= is ""

query' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
query' cid prog =
  readCBOR >=> queryResponse $ rec
    [ "request_type" =: GText "query"
    , "sender" =: GBlob defaultUser
    , "canister_id" =: GBlob cid
    , "method_name" =: GText "query"
    , "arg" =: GBlob (run prog)
    ]

query :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO Blob
query cid prog = query' cid prog >>= isReply

query_ :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ()
query_ cid prog = query cid prog >>= is ""

-- Shortcut for test cases that just need one canister
simpleTestCase :: HasTestConfig => String -> (Blob -> IO ()) -> TestTree
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

type HasTestConfig = (?testConfig :: TestConfig)

withTestConfig :: (forall. HasTestConfig => TestTree) -> TestConfig -> TestTree
withTestConfig act tc = let ?testConfig = tc in act

testConfig :: HasTestConfig => TestConfig
testConfig = ?testConfig

endPoint :: HasTestConfig => String
endPoint = tc_endPoint testConfig

testManager :: HasTestConfig => Manager
testManager = tc_manager testConfig

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

-- Other utilities

asHex :: Blob -> String
asHex = T.unpack . H.encodeHex . BS.toStrict

shorten :: Int -> String -> String
shorten n s = a ++ (if null b then "" else "…")
  where (a,b) = splitAt n s
