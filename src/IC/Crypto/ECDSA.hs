{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IC.Crypto.ECDSA
 ( SecretKey
 , createKey
 , toPublicKey
 , IC.Crypto.ECDSA.sign
 , IC.Crypto.ECDSA.verify
 ) where

import qualified Data.ByteString.Lazy as BS
import Crypto.ECC
import Crypto.Error
import Crypto.Random
import Crypto.PubKey.ECDSA
import Crypto.Hash.Algorithms
import Data.Proxy
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.Hashable

newtype SecretKey = SecretKey (KeyPair Curve_P256R1)
  deriving Show

deriving instance Show (KeyPair Curve_P256R1)


createKey :: BS.ByteString -> SecretKey
createKey seed =
    SecretKey $ fst $ withDRG drg (curveGenerateKeyPair Proxy)
  where
    drg = drgNewSeed $ seedFromInteger $ fromIntegral $ hash seed

toPublicKey :: SecretKey -> BS.ByteString
toPublicKey (SecretKey kp) =
    BS.fromStrict $ encodePublic (Proxy @Curve_P256R1) $ keypairGetPublic kp

sign :: SecretKey -> BS.ByteString -> IO BS.ByteString
sign (SecretKey kp) msg = do
    (r,s) <- signatureToIntegers Proxy <$>
        Crypto.PubKey.ECDSA.sign (Proxy @Curve_P256R1) (keypairGetPrivate kp) SHA256 (BS.toStrict msg)
    return $ encodeASN1 DER
        [ Start Sequence
        , IntVal r
        , IntVal s
        , End Sequence
        ]

verify :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool
verify pk msg sig
 | CryptoPassed pk <- decodePublic (Proxy @Curve_P256R1) (BS.toStrict pk)
 , Right [Start Sequence, IntVal r, IntVal s, End Sequence] <- decodeASN1 DER sig
 , CryptoPassed sig <- signatureFromIntegers (Proxy @Curve_P256R1) (r, s)
 = Crypto.PubKey.ECDSA.verify
    (Proxy @Curve_P256R1)
    SHA256
    pk
    sig
    (BS.toStrict msg)
 | otherwise = False

