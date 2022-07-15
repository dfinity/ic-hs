{- |

This module contains function to interact with an Internet Computer instance.

The primary customer here is IC.Test.Spec, i.e the test suite. Therefore, the
functions here give access to varoius levels of abstractions, and gives more control
than a “normal” agent library would do.

Also, because of the focus on testing, failures are repoted directly with HUnit’s 'assertFailure'.

As guidance: This modules does _not_ rely on the universal canister.

This module can also be used in a REPL; see 'connect'.

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}
module IC.Test.Agent
    (
      HTTPErrOr,
      HasAgentConfig,
      IC00,
      ReqResponse(..),
      ReqStatus(..),
      AgentConfig(..),
      addExpiry,
      addNonce,
      addNonceExpiryEnv,
      anonymousUser,
      as2Word64,
      asHex,
      asRight,
      asWord128,
      asWord32,
      asWord64,
      awaitCall',
      awaitCall,
      awaitKnown,
      awaitStatus,
      bothSame,
      certValue,
      certValueAbsent,
      code202,
      code202_or_4xx,
      code2xx,
      code4xx,
      connect,
      decodeCert',
      defaultSK,
      defaultUser,
      delegationEnv,
      doesn'tExist,
      ecdsaSK,
      ecdsaUser,
      enum,
      envelope,
      envelopeFor,
      extractCertData,
      getRequestStatus,
      getStateCert',
      getStateCert,
      ic00,
      ic00as,
      ingressDelay,
      is2xx,
      isErrOrReject,
      isPendingOrProcessing,
      isReject,
      isReply,
      okCBOR,
      otherSK,
      otherUser,
      makeAgentConfig,
      postCBOR,
      postCallCBOR,
      postQueryCBOR,
      postReadStateCBOR,
      preFlight,
      queryCBOR,
      queryResponse,
      runGet,
      secp256k1SK,
      secp256k1User,
      senderOf,
      shorten,
      submitCall,
      textual,
      validateStateCert,
      verifySignature,
      webAuthnECDSASK,
      webAuthnECDSAUser,
      webAuthnRSASK,
      webAuthnRSAUser,
      withAgentConfig,

      -- TODO: these are needed by IC.Test.Agent.Calls. Consider moving them to an Internal module
      callIC,
      callIC',
      callIC'',
      agentConfig,
    )
    where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Text.Hex as H
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.HashMap.Lazy as HM
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Numeric.Natural
import Data.Char
import Test.Tasty.HUnit
import Test.Tasty.Options
import Control.Monad.Except
import Control.Concurrent
import Control.Exception (catch)
import Data.Traversable
import Data.Word
import GHC.TypeLits
import System.Random
import System.Exit
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Codec.Candid (Principal(..), prettyPrincipal)
import qualified Data.Binary as Get
import qualified Data.Binary.Get as Get
import qualified Codec.Candid as Candid
import Data.Bits
import Data.Row
import qualified Data.Row.Variants as V
import qualified Haskoin.Crypto.Signature as Haskoin
import qualified Haskoin.Crypto.Hash as Haskoin
import qualified Haskoin.Keys.Common as Haskoin

import IC.Version
import IC.HTTP.GenR
import IC.HTTP.GenR.Parse
import IC.HTTP.CBOR (decode, encode)
import IC.HTTP.RequestId
import IC.Management
import IC.Crypto
import qualified IC.Crypto.DER as DER
import qualified IC.Crypto.DER_BLS as DER_BLS
import IC.Id.Forms
import IC.Test.Options
import IC.HashTree hiding (Blob, Label)
import IC.Certificate
import IC.Certificate.Value
import IC.Certificate.CBOR

-- * Agent configuration

data AgentConfig = AgentConfig
    { tc_root_key :: Blob
    , tc_manager :: Manager
    , tc_endPoint :: String
    , tc_test_port :: Int
    , tc_timeout :: Integer
    }

makeAgentConfig :: String -> Int -> Integer -> IO AgentConfig
makeAgentConfig ep' tp to = do
    manager <- newTlsManagerWith $ tlsManagerSettings
      { managerResponseTimeout = responseTimeoutMicro 60_000_000 -- 60s
      }
    request <- parseRequest $ ep ++ "/api/v2/status"
    putStrLn $ "Fetching endpoint status from " ++ show ep ++ "..."
    s <- (httpLbs request manager >>= okCBOR >>= statusResonse)
        `catch` (\(HUnitFailure _ r) -> putStrLn r >> exitFailure)

    putStrLn $ "Spec version tested:  " ++ T.unpack specVersion
    putStrLn $ "Spec version claimed: " ++ T.unpack (status_api_version s)

    return AgentConfig
        { tc_root_key = status_root_key s
        , tc_manager = manager
        , tc_endPoint = ep
        , tc_test_port = tp
        , tc_timeout = to
        }
  where
    -- strip trailing slash
    ep | null ep'        = error "empty endpoint"
       | last ep' == '/' = init ep'
       | otherwise       = ep'

preFlight :: OptionSet -> IO AgentConfig
preFlight os = do
    let Endpoint ep = lookupOption os
    let TestPort tp = lookupOption os
    let PollTimeout to = lookupOption os
    makeAgentConfig ep tp to


newtype ReplWrapper = R (forall a. (HasAgentConfig => a) -> a)

-- |  This is for use from the Haskell REPL, see README.md
connect :: String -> Int -> Integer -> IO ReplWrapper
connect ep tp to = do
    agentConfig <- makeAgentConfig ep tp to
    let ?agentConfig = agentConfig
    return (R $ \x -> x)

-- Yes, implicit arguments are frowned upon. But they are also very useful.

type HasAgentConfig = (?agentConfig :: AgentConfig)

withAgentConfig :: (forall. HasAgentConfig => a) -> AgentConfig -> a
withAgentConfig act tc = let ?agentConfig = tc in act

agentConfig :: HasAgentConfig => AgentConfig
agentConfig = ?agentConfig

endPoint :: HasAgentConfig => String
endPoint = tc_endPoint agentConfig

agentManager :: HasAgentConfig => Manager
agentManager = tc_manager agentConfig

-- * Test data for some hardcoded user names

doesn'tExist :: Blob
doesn'tExist = "\xDE\xAD\xBE\xEF" -- hopefully no such canister/user exists

defaultSK :: SecretKey
defaultSK = createSecretKeyEd25519 "fixed32byteseedfortesting"

otherSK :: SecretKey
otherSK = createSecretKeyEd25519 "anotherfixed32byteseedfortesting"

webAuthnECDSASK :: SecretKey
webAuthnECDSASK = createSecretKeyWebAuthnECDSA "webauthnseed"

webAuthnRSASK :: SecretKey
webAuthnRSASK = createSecretKeyWebAuthnRSA "webauthnseed"

ecdsaSK :: SecretKey
ecdsaSK = createSecretKeyECDSA "ecdsaseed"

secp256k1SK :: SecretKey
secp256k1SK = createSecretKeySecp256k1 "secp256k1seed"

defaultUser :: Blob
defaultUser = mkSelfAuthenticatingId $ toPublicKey defaultSK
otherUser :: Blob
otherUser = mkSelfAuthenticatingId $ toPublicKey otherSK
webAuthnECDSAUser :: Blob
webAuthnECDSAUser = mkSelfAuthenticatingId $ toPublicKey webAuthnECDSASK
webAuthnRSAUser :: Blob
webAuthnRSAUser = mkSelfAuthenticatingId $ toPublicKey webAuthnRSASK
ecdsaUser :: Blob
ecdsaUser = mkSelfAuthenticatingId $ toPublicKey ecdsaSK
secp256k1User :: Blob
secp256k1User = mkSelfAuthenticatingId $ toPublicKey secp256k1SK

anonymousUser :: Blob
anonymousUser = "\x04"

-- * Request envelopes

addIfNotThere :: Monad m => T.Text -> m GenR -> GenR -> m GenR
addIfNotThere f _ (GRec hm)| f `HM.member` hm = return (GRec hm)
addIfNotThere f a (GRec hm) = do
  x <- a
  return $ GRec $ HM.insert f x hm
addIfNotThere _ _ _ = error "addIfNotThere: not a record"

addNonce :: GenR -> IO GenR
addNonce = addIfNotThere "nonce" $
    GBlob <$> getRand8Bytes

getRand8Bytes :: IO BS.ByteString
getRand8Bytes = BS.pack <$> replicateM 8 randomIO

-- Adds expiry 1 minute
addExpiry :: GenR -> IO GenR
addExpiry = addIfNotThere "ingress_expiry" $ do
    t <- getPOSIXTime
    return $ GNat $ round ((t + 60) * 1000_000_000)

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

-- a little bit of smartness in our combinators: Adding correct envelope, nonce
-- and expiry if it is not already there

senderOf :: GenR -> Blob
senderOf (GRec hm) | Just (GBlob id) <- HM.lookup "sender" hm = id
senderOf _ = anonymousUser

addNonceExpiryEnv :: GenR -> IO GenR
addNonceExpiryEnv req = do
  addNonce req >>= addExpiry >>= envelopeFor (senderOf req)

envelopeFor :: Blob -> GenR -> IO GenR
envelopeFor u content | u == anonymousUser = return $ rec [ "content" =: content ]
envelopeFor u content = envelope key content
  where
    key ::  SecretKey
    key | u == defaultUser = defaultSK
        | u == otherUser = otherSK
        | u == webAuthnECDSAUser = webAuthnECDSASK
        | u == webAuthnRSAUser = webAuthnRSASK
        | u == ecdsaUser = ecdsaSK
        | u == secp256k1User = secp256k1SK
        | u == anonymousUser = error "No key for the anonymous user"
        | otherwise = error $ "Don't know key for user " ++ show u

-- * HUnit error reporting integration

asRight :: HasCallStack => Either T.Text a -> IO a
asRight (Left err) = assertFailure (T.unpack err)
asRight (Right gr) = return gr

asExceptT :: HasCallStack => ExceptT T.Text IO a -> IO a
asExceptT act = runExceptT act >>= asRight

-- * Requests

-- | Posting a CBOR request, returning a raw bytestring
postCBOR :: (HasCallStack, HasAgentConfig) => String -> GenR -> IO (Response BS.ByteString)
postCBOR path gr = do
    request <- parseRequest $ endPoint ++ path
    request <- return $ request
      { method = "POST"
      , requestBody = RequestBodyLBS $ BS.toLazyByteString $ encode gr
      , requestHeaders = [(hContentType, "application/cbor")]
      }
    httpLbs request agentManager

-- | postCBOR with url based on effective canister id
postCallCBOR, postQueryCBOR, postReadStateCBOR :: (HasCallStack, HasAgentConfig) => Blob -> GenR -> IO (Response BS.ByteString)
postCallCBOR cid      = postCBOR $ "/api/v2/canister/" ++ textual cid ++ "/call"
postQueryCBOR cid     = postCBOR $ "/api/v2/canister/" ++ textual cid ++ "/query"
postReadStateCBOR cid = postCBOR $ "/api/v2/canister/" ++ textual cid ++ "/read_state"

-- | Add envelope to CBOR request, add a nonce and expiry if it is not there,
-- post to "read", return decoded CBOR
queryCBOR :: (HasCallStack, HasAgentConfig) => Blob -> GenR -> IO GenR
queryCBOR cid req = do
  addNonceExpiryEnv req >>= postQueryCBOR cid >>= okCBOR

type HTTPErrOr a = Either (Int,String) a

-- | Add envelope to CBOR, and a nonce and expiry if not there, post to
-- "submit". Returns either a HTTP Error code, or if the status is 2xx, the
-- request id.
submitCall' :: (HasCallStack, HasAgentConfig) => Blob -> GenR -> IO (HTTPErrOr (IO ReqStatus))
submitCall' cid req = do
  req <- addNonce req
  req <- addExpiry req
  res <- envelopeFor (senderOf req) req >>= postCallCBOR cid
  let code = statusCode (responseStatus res)
  if 200 <= code && code < 300
  then do
     assertBool "Response body not empty" (BS.null (responseBody res))
     pure $ Right (getRequestStatus (senderOf req) cid (requestId req))
  else do
    let msg = T.unpack (T.decodeUtf8With T.lenientDecode (BS.toStrict (BS.take 200 (responseBody res))))
    pure $ Left (code, msg)

submitCall :: (HasCallStack, HasAgentConfig) => Blob -> GenR -> IO (IO ReqStatus)
submitCall cid req = submitCall' cid req >>= is2xx

-- | Add envelope to CBOR, and a nonce and expiry if not there, post to
-- "submit". Returns either a HTTP Error code, or if the status is 2xx, poll
-- for the request response, and return decoded CBOR
awaitCall' :: (HasCallStack, HasAgentConfig) => Blob -> GenR -> IO (HTTPErrOr ReqResponse)
awaitCall' cid req = do
  submitCall' cid req >>= \case
    Left e -> pure (Left e)
    Right getStatus -> Right <$> awaitStatus getStatus

-- | Add envelope to CBOR, and a nonce and expiry if not there, post to
-- "submit", poll for the request response, and return decoded CBOR
awaitCall :: (HasCallStack, HasAgentConfig) => Blob -> GenR -> IO ReqResponse
awaitCall cid req = awaitCall' cid req >>= is2xx

is2xx :: HasCallStack => HTTPErrOr a -> IO a
is2xx = \case
    Left (c,msg) -> assertFailure $ "Status " ++ show c ++ " is not 2xx:\n" ++ msg
    Right res -> pure res

getStateCert' :: (HasCallStack, HasAgentConfig) => Blob -> Blob -> [[Blob]] -> IO (Response Blob)
getStateCert' sender ecid paths = do
    req <- addExpiry $ rec
      [ "request_type" =: GText "read_state"
      , "sender" =: GBlob sender
      , "paths" =: GList (map (GList . map GBlob) paths)
      ]
    envelopeFor (senderOf req) req >>= postReadStateCBOR ecid

decodeCert' :: HasCallStack => Blob -> IO Certificate
decodeCert' b = either (assertFailure . T.unpack) return $ decodeCert b

getStateCert :: (HasCallStack, HasAgentConfig) => Blob -> Blob -> [[Blob]] -> IO Certificate
getStateCert sender ecid paths = do
    gr <- getStateCert' sender ecid paths >>= okCBOR
    b <- asExceptT $ record (field blob "certificate") gr
    cert <- decodeCert' b

    case wellFormed (cert_tree cert) of
        Left err -> assertFailure $ "Hash tree not well formed: " ++ err
        Right () -> return ()

    return cert

extractCertData :: Blob -> Blob -> IO Blob
extractCertData cid b = do
  cert <- decodeCert' b
  case wellFormed (cert_tree cert) of
      Left err -> assertFailure $ "Hash tree not well formed: " ++ err
      Right () -> return ()
  certValue cert ["canister", cid, "certified_data"]

verboseVerify :: String -> Blob -> Blob -> Blob -> Blob -> IO ()
verboseVerify what domain_sep pk msg sig =
    case DER_BLS.verify domain_sep pk msg sig of
        Left err -> assertFailure $ unlines
            [ "Signature verification failed on " ++ what
            , T.unpack err
            , "Domain separator:   " ++ prettyBlob domain_sep
            , "Public key (DER):   " ++ asHex pk
            , "Public key decoded: " ++
               case DER.decode pk of
                 Left err -> T.unpack err
                 Right (suite, key) -> asHex key ++ " (" ++ show suite ++ ")"
            , "Signature:          " ++ asHex sig
            , "Checked message:    " ++ prettyBlob msg
            ]
        Right () -> return ()

validateDelegation :: (HasCallStack, HasAgentConfig) => Maybe Delegation -> IO Blob
validateDelegation Nothing = return (tc_root_key agentConfig)
validateDelegation (Just del) = do
    cert <- decodeCert' (del_certificate del)
    case wellFormed (cert_tree cert) of
        Left err -> assertFailure $ "Hash tree not well formed: " ++ err
        Right () -> return ()
    validateStateCert' "certificate delegation" cert

    certValue cert ["subnet", del_subnet_id del, "public_key"]

validateStateCert' :: (HasCallStack, HasAgentConfig) => String -> Certificate -> IO ()
validateStateCert' what cert = do
    pk <- validateDelegation (cert_delegation cert)
    verboseVerify what "ic-state-root" pk (reconstruct (cert_tree cert)) (cert_sig cert)

validateStateCert :: (HasCallStack, HasAgentConfig) => Certificate -> IO ()
validateStateCert = validateStateCert' "certificate"

data ReqResponse = Reply Blob | Reject Natural T.Text (Maybe T.Text)
  deriving (Eq, Show)
data ReqStatus = Processing | Pending | Responded ReqResponse | UnknownStatus
  deriving (Eq, Show)

prettyPath :: [Blob] -> String
prettyPath = concatMap (("/" ++) . shorten 15 . prettyBlob)

prettyBlob :: Blob -> String
prettyBlob x =
  let s = map (chr . fromIntegral) (BS.unpack x) in
  if all isPrint s then s else asHex x

maybeCertValue :: HasCallStack => CertVal a => Certificate -> [Blob] -> IO (Maybe a)
maybeCertValue cert path = case lookupPath (cert_tree cert) path of
    Found b -> case fromCertVal b of
      Just x -> return (Just x)
      Nothing -> assertFailure $ "Cannot parse " ++ prettyPath path ++ " from " ++ show b
    Absent -> return Nothing
    x -> assertFailure $ "Expected to find " ++ prettyPath path ++ ", but got " ++ show x

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

getRequestStatus :: (HasCallStack, HasAgentConfig) => Blob -> Blob -> Blob -> IO ReqStatus
getRequestStatus sender cid rid = do
    cert <- getStateCert sender cid [["request_status", rid]]

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
        errorCode <- maybeCertValue cert ["request_status", rid, "error_code"]
        return $ Responded (Reject code msg errorCode)
      Found s -> assertFailure $ "Unexpected status " ++ show s
      -- This case should not happen with a compliant IC, but let
      -- us be liberal here, and strict in a dedicated test
      Unknown -> return UnknownStatus
      x -> assertFailure $ "Unexpected request status, got " ++ show x

loop :: (HasCallStack, HasAgentConfig) => IO (Maybe a) -> IO a
loop act = getCurrentTime >>= go
  where
    go init = act >>= \case
      Just r -> return r
      Nothing -> do
        now <- getCurrentTime
        if diffUTCTime now init > fromInteger (tc_timeout agentConfig) then assertFailure "Polling timed out"
        else go init

awaitStatus :: HasAgentConfig => IO ReqStatus -> IO ReqResponse
awaitStatus get_status = loop $ pollDelay >> get_status >>= \case
  Responded x -> return $ Just x
  _ -> return Nothing

-- Polls until status is not Unknown any more, and returns that status
-- even if Pending or Processing
awaitKnown :: HasAgentConfig => IO ReqStatus -> IO ReqStatus
awaitKnown get_status = loop $ pollDelay >> get_status >>= \case
  UnknownStatus -> return Nothing
  x -> return $ Just x

isPendingOrProcessing :: ReqStatus -> IO ()
isPendingOrProcessing Pending = return ()
isPendingOrProcessing Processing = return ()
isPendingOrProcessing r = assertFailure $ "Expected pending or processing, got " <> show r

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
queryResponse = asExceptT . record do
    s <- field text "status"
    case s of
      "replied" ->
        Reply <$> field (record (field blob "arg")) "reply"
      "rejected" -> do
        code <- field nat "reject_code"
        msg <- field text "reject_message"
        error_code <- optionalField text "error_code"
        return $ Reject code msg error_code
      _ -> lift $ throwError $ "Unexpected status " <> T.pack (show s)

isReject :: HasCallStack => [Natural] -> ReqResponse -> IO ()
isReject _ (Reply r) =
  assertFailure $ "Expected reject, got reply:" ++ prettyBlob r
isReject codes (Reject n msg _) = do
  assertBool
    ("Reject code " ++ show n ++ " not in " ++ show codes ++ "\n" ++ T.unpack msg)
    (n `elem` codes)

isErrOrReject :: HasCallStack => [Natural] -> HTTPErrOr ReqResponse -> IO ()
isErrOrReject _codes (Left (c, msg))
    | 400 <= c && c < 600 = return ()
    | otherwise = assertFailure $
        "Status " ++ show c ++ " is not 4xx or 5xx:\n" ++ msg
isErrOrReject [] (Right _) = assertFailure "Got HTTP response, expected HTTP error"
isErrOrReject codes (Right res) = isReject codes res


isReply :: HasCallStack => ReqResponse -> IO Blob
isReply (Reply b) = return b
isReply (Reject n msg error_code) =
  assertFailure $ "Unexpected reject (code " ++ show n ++ (maybe "" showErrCode error_code) ++ "): " ++ T.unpack msg
  where showErrCode ec = ", error_code: " ++ T.unpack ec

-- Convenience decoders

asWord32 :: HasCallStack => Blob -> IO Word32
asWord32 = runGet Get.getWord32le

asWord64 :: HasCallStack => Blob -> IO Word64
asWord64 = runGet Get.getWord64le

as2Word64 :: HasCallStack => Blob -> IO (Word64, Word64)
as2Word64 = runGet $ (,) <$> Get.getWord64le <*> Get.getWord64le

asWord128 :: HasCallStack => Blob -> IO Natural
asWord128 = runGet $ do
    low <- Get.getWord64le
    high <- Get.getWord64le
    return $ fromIntegral high `shiftL` 64 .|. fromIntegral low

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

-- * Status endpoint parsing

data StatusResponse = StatusResponse
    { status_api_version :: T.Text
    , status_root_key :: Blob
    }

statusResonse :: HasCallStack => GenR -> IO StatusResponse
statusResonse = asExceptT . record do
    v <- field text "ic_api_version"
    _ <- optionalField text "impl_source"
    _ <- optionalField text "impl_version"
    _ <- optionalField text "impl_revision"
    pk <- field blob "root_key"
    swallowAllFields -- More fields are explicitly allowed
    return StatusResponse {status_api_version = v, status_root_key = pk }

-- * Interacting with aaaaa-aa (via HTTP)

{-
The code below has some repetition. That’s because we have

 A) multiple ways of _calling_ the Management Canister
    (as default user, as specific user, via canister, with or without cycles),
 B) different things we want to know
    (just the Candid-decoded reply, or the response, or even the HTTP error)
 C) and then of course different methods (which affect response decoding)

So far, there is some duplication here because of that. Eventually, this should
be refactored so that the test can declarative pick A, B and C separately.
-}

-- how to reach the management canister
type IC00 = Blob -> T.Text -> Blob -> IO ReqResponse

ic00as :: (HasAgentConfig, HasCallStack) => Blob -> IC00
ic00as user ecid method_name arg = awaitCall ecid $ rec
      [ "request_type" =: GText "call"
      , "sender" =: GBlob user
      , "canister_id" =: GBlob ""
      , "method_name" =: GText method_name
      , "arg" =: GBlob arg
      ]

ic00 :: HasAgentConfig => IC00
ic00 = ic00as defaultUser

-- A variant that allows non-200 responses to submit
ic00as' :: HasAgentConfig => Blob -> Blob -> T.Text -> Blob -> IO (HTTPErrOr ReqResponse)
ic00as' user cid method_name arg = awaitCall' cid $ rec
      [ "request_type" =: GText "call"
      , "sender" =: GBlob user
      , "canister_id" =: GBlob ""
      , "method_name" =: GText method_name
      , "arg" =: GBlob arg
      ]

-- Now wrapping the concrete calls
-- (using Candid.toCandidService is tricky because of all stuff like passing through the effective canister id)
--
callIC :: forall s a b.
  (HasCallStack, HasAgentConfig) =>
  KnownSymbol s =>
  (a -> IO b) ~ (ICManagement IO .! s) =>
  (Candid.CandidArg a, Candid.CandidArg b) =>
  IC00 -> Blob -> Label s -> a -> IO b
callIC ic00 ecid l x = do
    r <- ic00 ecid (T.pack (symbolVal l)) (Candid.encode x) >>= isReply
    case Candid.decode r of
        Left err -> assertFailure $ "Candid decoding error: " ++ err
        Right y -> pure y

-- Primed variants return the response (reply or reject)
callIC' :: forall s a b.
  HasAgentConfig =>
  KnownSymbol s =>
  (a -> IO b) ~ (ICManagement IO .! s) =>
  Candid.CandidArg a =>
  IC00 -> Blob -> Label s -> a -> IO ReqResponse
callIC' ic00 ecid l x = ic00 ecid (T.pack (symbolVal l)) (Candid.encode x)

-- Double primed variants are only for requests from users (so they take the user,
-- not a generic ic00 thing), and return the HTTP error code or the response
-- (reply or reject)

callIC'' :: forall s a b.
  HasAgentConfig =>
  KnownSymbol s =>
  (a -> IO b) ~ (ICManagement IO .! s) =>
  Candid.CandidArg a =>
  Blob -> Blob -> Label s -> a -> IO (HTTPErrOr ReqResponse)
callIC'' user ecid l x = ic00as' user ecid (T.pack (symbolVal l)) (Candid.encode x)

-- Convenience around Data.Row.Variants used as enums

enum :: (AllUniqueLabels r, KnownSymbol l, (r .! l) ~ ()) => Label l -> Var r
enum l = V.IsJust l ()

-- Other utilities

asHex :: Blob -> String
asHex = T.unpack . H.encodeHex . BS.toStrict

textual :: Blob -> String
textual = T.unpack . prettyPrincipal . Principal

shorten :: Int -> String -> String
shorten n s = a ++ (if null b then "" else "…")
  where (a,b) = splitAt n s

toHash256 :: Blob -> Haskoin.Hash256
toHash256 = Get.runGet Get.get

verifySignature :: Blob -> Blob -> Blob -> Bool
verifySignature msg sig key = Haskoin.verifyHashSig (toHash256 msg) s pk
  where Just pk = Haskoin.importPubKey $ BS.toStrict key
        Just s  = Haskoin.decodeStrictSig $ BS.toStrict sig
