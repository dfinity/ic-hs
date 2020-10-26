{- |
Everything related to signature creation and checking
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IC.Crypto
 ( SecretKey
 , createSecretKeyEd25519
 , createSecretKeyEd25519Raw
 , createSecretKeyWebAuthn
 , createSecretKeyECDSA
 , toPublicKey
 , sign
 , verify
 )
 where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified IC.Crypto.Ed25519 as Ed25519
import qualified IC.Crypto.DER as DER
import qualified IC.Crypto.WebAuthn as WebAuthn
import qualified IC.Crypto.ECDSA as ECDSA
import Data.Int
import Data.Bifunctor
import Control.Monad.Except

data SecretKey
    = Ed25519 Ed25519.SecretKey
    | Ed25519Raw Ed25519.SecretKey -- non-DER encoded public keys
    | ECDSA ECDSA.SecretKey
    | WebAuthn WebAuthn.SecretKey
  deriving Show

createSecretKeyEd25519 :: BS.ByteString -> SecretKey
createSecretKeyEd25519 = Ed25519 . Ed25519.createKey

createSecretKeyEd25519Raw :: BS.ByteString -> SecretKey
createSecretKeyEd25519Raw = Ed25519Raw . Ed25519.createKey

createSecretKeyWebAuthn :: BS.ByteString -> SecretKey
createSecretKeyWebAuthn = WebAuthn . WebAuthn.createKey

createSecretKeyECDSA :: BS.ByteString -> SecretKey
createSecretKeyECDSA = ECDSA . ECDSA.createKey

toPublicKey :: SecretKey -> BS.ByteString
toPublicKey (Ed25519Raw sk) = Ed25519.toPublicKey sk
toPublicKey (Ed25519 sk) = DER.encode DER.Ed25519 $ Ed25519.toPublicKey sk
toPublicKey (WebAuthn sk) = DER.encode DER.WebAuthn $ WebAuthn.toPublicKey sk
toPublicKey (ECDSA sk) = DER.encode DER.ECDSA $ ECDSA.toPublicKey sk

sign :: BS.ByteString -> SecretKey -> BS.ByteString -> IO BS.ByteString
sign domain_sep sk payload = case sk of
    Ed25519 sk -> return $ Ed25519.sign sk msg
    Ed25519Raw sk -> return $ Ed25519.sign sk msg
    WebAuthn sk -> WebAuthn.sign sk msg
    ECDSA sk -> ECDSA.sign sk msg
  where
    msg = BS.singleton (fromIntegral (BS.length domain_sep)) <> domain_sep <> payload

unpack :: BS.ByteString -> Either T.Text (DER.Suite, BS.ByteString)
unpack pk | BS.take 1 pk == "\x30" = first T.pack $ DER.decode pk
          | otherwise = Right (DER.Ed25519, pk) -- raw format

verify :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either T.Text ()
verify domain_sep der_pk payload sig = unpack der_pk >>= \case
  (DER.WebAuthn, pk) -> do
    unless (WebAuthn.verify pk msg sig) $ do
        when (WebAuthn.verify pk payload sig) $
            throwError $ "domain separator " <> T.pack (show domain_sep) <> " missing"
        throwError "signature verification failed"

  (DER.Ed25519, pk) -> do
    assertLen "Ed25519 public key" 32 pk
    assertLen "Ed25519 signature" 64 sig

    unless (Ed25519.verify pk msg sig) $ do
        when (Ed25519.verify pk payload sig) $
            throwError $ "domain separator " <> T.pack (show domain_sep) <> " missing"
        throwError "signature verification failed"

  (DER.ECDSA, pk) -> do
    unless (ECDSA.verify pk msg sig) $ do
        when (ECDSA.verify pk payload sig) $
            throwError $ "domain separator " <> T.pack (show domain_sep) <> " missing"
        throwError "signature verification failed"
  where
    msg = BS.singleton (fromIntegral (BS.length domain_sep)) <> domain_sep <> payload


assertLen :: T.Text -> Int64 -> BS.ByteString -> Either T.Text ()
assertLen what len bs
  | BS.length bs == len = return ()
  | otherwise = throwError $ what <> " has wrong length " <>  T.pack (show (BS.length bs)) <> ", expected " <> T.pack (show len)
