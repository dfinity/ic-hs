{-|
This module implements WebAuthN crypto. WebauthN is a big mess, involving
nesting of CBOR, DER and JSON…

For signature _checking_, we implement the necessary steps directly here, in
pure Haskell. This means that `ic-ref` can run independent of external tools.

We do not use the `webauthn` library from hackage (it does additional checks),
although some of the code here is based on that.

For signature _creation_, we use the Python implementation in
https://github.com/dfinity-lab/ic-webauthn-cli, purely out of laziness. This
could be changed.

Until then, `ic-ref-test` will expect `ic-webauthn-cli` on
the `PATH`, but that’s ok, because `ic-ref-test` is only used in nixified
environment where this can be ensured seamlessly.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
module IC.Crypto.WebAuthn
 ( init
 , SecretKey
 , createKey
 , toPublicKey
 , sign
 , verify
 ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO.Temp
import System.IO.Unsafe
import System.IO
import System.Exit
import System.Process.ByteString.Lazy
import Data.Bifunctor
import Control.Monad
import Data.Hashable
import Data.ByteString.Base64.URL.Lazy as Base64
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified IC.HTTP.CBOR as CBOR
import Codec.CBOR.Term
import Codec.CBOR.Read
import IC.CBORPatterns
import qualified Data.Map as M
import IC.HTTP.GenR.Parse
import IC.Hash
import Control.Monad.Except
import qualified Crypto.PubKey.ECC.ECDSA as EC
import qualified Crypto.PubKey.ECC.Types as EC
import qualified Crypto.Number.Serialize as EC
import Crypto.Hash.Algorithms (SHA256(..))
import Data.ASN1.Types
import IC.Crypto.DER.Decode

newtype SecretKey = SecretKey BS.ByteString
  deriving Show

withSecretKey :: SecretKey -> (FilePath -> IO a) -> IO a
withSecretKey (SecretKey sk) act =
  withSystemTempFile "webauthn-seckey.pem" $ \fn h -> BS.hPut h sk >> hClose h >> act fn

toPublicKey :: SecretKey -> BS.ByteString
toPublicKey sk = unsafePerformIO $ do
  (code, out, err) <- withSecretKey sk $ \fn ->
     readProcessWithExitCode "ic-webauthn-cli" [ "get_public_key", fn ] mempty
  unless (code == ExitSuccess) $
    die $ "ic-webauthn-cli create failed: " ++ show code ++ "\n" ++ BS.unpack err
  return out

createKey :: BS.ByteString -> SecretKey
createKey seed = unsafePerformIO $ do
  -- seed is actually hex, but hey, decimal is also hex
  (code, out, err) <- readProcessWithExitCode
    "ic-webauthn-cli" [ "create" ] (BS.pack $ show (abs (hash seed) + 1) ++ "\n")
  unless (code == ExitSuccess) $
    die $ "ic-webauthn-cli create failed: " ++ show code ++ "\n" ++ BS.unpack err
  return (SecretKey out)

sign :: SecretKey -> BS.ByteString -> IO BS.ByteString
sign sk msg = do
  (code, out, err) <- withSecretKey sk $ \fn ->
    readProcessWithExitCode "ic-webauthn-cli" [ "sign", fn ] msg
  unless (code == ExitSuccess) $
    die $ "ic-webauthn-cli sign failed: " ++ show code ++ "\n" ++ BS.unpack err
  return out

parseSig :: BS.ByteString -> Either T.Text (BS.ByteString, BS.ByteString, BS.ByteString)
parseSig = CBOR.decode >=> record do
      ad <- field blob "authenticator_data"
      cdj <- BS.fromStrict . T.encodeUtf8 <$> field text "client_data_json"
      sig <- field blob "signature"
      return (ad, cdj, sig)

parseClientDataJson :: BS.ByteString -> Either T.Text BS.ByteString
parseClientDataJson blob = first T.pack $
    JSON.eitherDecode blob >>= JSON.parseEither p
  where
    p = JSON.withObject "clientData" $ \o -> do
      x <- o JSON..: "challenge"
      either JSON.parseFail return $ Base64.decodeUnpadded (BS.pack x)

parseCOSEKey :: BS.ByteString -> Either T.Text EC.PublicKey
parseCOSEKey s =
    first (\(DeserialiseFailure _ s) -> "CBOR decoding failure: " <> T.pack s)
        (deserialiseFromBytes decodeTerm s)
    >>= go
  where
    go (_, TMap_ kv) = do
      m <- M.fromList <$> mapM keyVal kv
      let field n = case M.lookup n m of
              Just x -> return x
              Nothing -> throwError $ "COSE: missing entry " <> T.pack (show n)
      let intField n = field n >>= \case
              TInt i -> pure i
              _ -> throwError $ "COSE field " <> T.pack (show n) <> " not an int"
      let bytesField n = field n >>= \case
              TBytes b -> pure b
              _ -> throwError $ "COSE field " <> T.pack (show n) <> " not bytes"

      ty <- intField 3
      unless (ty == -7) $
          throwError "COSE: Only type -7 (ECDSA) supported"
      crv <- intField (-1)
      unless (crv == 1) $
          throwError $ "parsePublicKey: unknown curve: " <> T.pack (show crv)
      xb <- bytesField (-2)
      yb <- bytesField (-3)
      let x = EC.os2ip xb
      let y = EC.os2ip yb
      return $ EC.PublicKey (EC.getCurveByName EC.SEC_p256r1) (EC.Point x y)

    go _ = throwError "COSE key not a CBOR map"

    keyVal (TInt k,v) = pure (fromIntegral k,v)
    keyVal (TInteger k,v) = pure (k,v)
    keyVal _ = throwError "Non-integer key in CBOR map"

parseCOSESig :: BS.ByteString -> Either T.Text EC.Signature
parseCOSESig s =
  first T.pack (safeDecode s) >>= \case
    Start Sequence:IntVal r:IntVal s:End Sequence:_ -> pure $ EC.Signature r s
    a -> throwError $ "Unexpected DER encoding for COSE sig: " <> T.pack (show a)

verify :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Either T.Text ()
verify pk msg sig = do
    (ad, cdj, sig) <- parseSig sig
    pk <- parseCOSEKey pk
    sig <- parseCOSESig sig
    unless (EC.verify SHA256 pk sig (BS.toStrict $ ad <> sha256 cdj)) $
      throwError "WebAuthn signature verification failed"
    challenge <- parseClientDataJson cdj
    unless (challenge == msg) $
      throwError $ "Wrong challenge. Expected " <> T.pack (show msg) <>
        " got " <> T.pack (show challenge)
