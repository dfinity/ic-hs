{- |
Everything related to signature creation and checking
-}
{-# LANGUAGE OverloadedStrings #-}
module IC.Crypto
 ( SecretKey
 , createSecretKey
 , toPublicKey
 , sign
 , verify
 )

 where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Crypto.Sign.Ed25519 as Ed25519
import Control.Monad.Except

type SecretKey = Ed25519.SecretKey

createSecretKey :: BS.ByteString -> SecretKey
createSecretKey seed | BS.length seed > 32 = error "Seed too long"
createSecretKey seed = sk
  where
    seed' = seed <> BS.replicate (32 - BS.length seed) 0x00
    Just (_, sk) = Ed25519.createKeypairFromSeed_ (BS.toStrict seed')


toPublicKey :: SecretKey -> BS.ByteString
toPublicKey = BS.fromStrict . Ed25519.unPublicKey . Ed25519.toPublicKey

sign :: BS.ByteString -> SecretKey -> BS.ByteString -> BS.ByteString
sign domain_sep sk payload =
    BS.fromStrict $ Ed25519.unSignature $ Ed25519.dsign sk $ BS.toStrict (domain_sep <> payload)


verify :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> Either T.Text ()
verify domain_sep pk payload sig = do
    unless (BS.length pk == 32) $
        throwError $ "public key has wrong length " <> T.pack (show (BS.length pk)) <> ", expected 32"
    let pk' = Ed25519.PublicKey (BS.toStrict pk)

    unless (BS.length sig == 64) $
        throwError $ "signature has wrong length " <> T.pack (show (BS.length pk)) <> ", expected 64"
    let sig' = Ed25519.Signature (BS.toStrict sig)

    when (Ed25519.dverify pk' (BS.toStrict payload) sig') $
        throwError $ "domain separator " <> T.pack (show domain_sep) <> " missing"

    unless (Ed25519.dverify pk' (BS.toStrict (domain_sep <> payload)) sig') $
        throwError "signature verification failed"


