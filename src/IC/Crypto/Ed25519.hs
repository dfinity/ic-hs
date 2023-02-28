module IC.Crypto.Ed25519
 ( SecretKey
 , createKey
 , toPublicKey
 , sign
 , verify
 ) where

import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as BS
import qualified Crypto.Sign.Ed25519 as Ed25519

type SecretKey = Ed25519.SecretKey

createKey :: BS.ByteString -> SecretKey
createKey seed | BS.length seed > 32 = error "Seed too long"
createKey seed = sk
  where
    seed' = seed <> BS.replicate (32 - BS.length seed) 0x00
    (_, sk) = fromJust $ Ed25519.createKeypairFromSeed_ (BS.toStrict seed')

toPublicKey :: SecretKey -> BS.ByteString
toPublicKey = BS.fromStrict . Ed25519.unPublicKey . Ed25519.toPublicKey

sign :: SecretKey -> BS.ByteString -> BS.ByteString
sign sk msg = BS.fromStrict $ Ed25519.unSignature $ Ed25519.dsign sk $ BS.toStrict msg

verify :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool
verify pk msg sig = Ed25519.dverify pk' msg' sig'
  where
    sig' = Ed25519.Signature (BS.toStrict sig)
    pk' = Ed25519.PublicKey (BS.toStrict pk)
    msg' = BS.toStrict msg
