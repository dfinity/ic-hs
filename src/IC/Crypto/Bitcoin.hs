module IC.Crypto.Bitcoin
 ( ExtendedSecretKey(..)
 , createExtendedKey
 , derivePrivateKey 
 , derivePublicKey
 , extractChainCode
 , publicKeyToDER
 , sign
 , toHash256
 , toWord32
 ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Binary as Get
import qualified Data.Binary.Get as Get
import qualified Haskoin.Keys.Common as Haskoin
import qualified Haskoin.Keys.Extended as Haskoin
import qualified Haskoin.Crypto.Signature as Haskoin
import qualified Haskoin.Crypto.Hash as Haskoin
import Data.Either.Combinators
import Data.Word

newtype ExtendedSecretKey = ExtendedSecretKey Haskoin.XPrvKey
  deriving Show

createExtendedKey :: BS.ByteString -> ExtendedSecretKey
createExtendedKey seed = ExtendedSecretKey $ Haskoin.makeXPrvKey $ BS.toStrict seed 

derivePrivateKey :: ExtendedSecretKey -> [BS.ByteString] -> Either String Haskoin.XPrvKey
derivePrivateKey (ExtendedSecretKey sk) path = mapRight (\p -> Haskoin.derivePath p sk) (parseSoftDerivationPath path)

derivePublicKey :: ExtendedSecretKey -> [BS.ByteString] -> Either String Haskoin.XPubKey
derivePublicKey (ExtendedSecretKey sk) path = mapRight (\p -> Haskoin.derivePubPath p (Haskoin.deriveXPubKey sk)) (parseSoftDerivationPath path)

sign :: Haskoin.XPrvKey -> Haskoin.Hash256 -> BS.ByteString
sign key msg = BS.fromStrict $ Haskoin.exportSig $ Haskoin.signHash (Haskoin.xPrvKey key) msg
 
parseSoftDerivationPath :: [BS.ByteString] -> Either String Haskoin.SoftPath
parseSoftDerivationPath l =
  case raw_path of
    Left err -> Left $ "Could not parse derivation path: " ++ err
    Right rp -> case Haskoin.toSoft $ Haskoin.listToPath rp of
                  Nothing -> Left $ "Could not soften derivation path"
                  Just p -> Right p 
  where
    raw_path = sequence $ map toWord32 l

publicKeyToDER :: Haskoin.XPubKey -> BS.ByteString
publicKeyToDER k = BS.fromStrict $ Haskoin.exportPubKey False $ Haskoin.xPubKey k

extractChainCode :: Haskoin.XPubKey -> BS.ByteString
extractChainCode k = BS.fromStrict $ BSS.fromShort $ Haskoin.getHash256 $ Haskoin.xPubChain k

toWord32 :: BS.ByteString -> Either String Word32
toWord32 = convert Get.getWord32be

toHash256 :: BS.ByteString -> Either String Haskoin.Hash256
toHash256 = Get.runGet Get.get 

convert :: Get.Get a -> BS.ByteString -> Either String a
convert get b = case (Get.runGetOrFail get b) of
  Left (_, _, err) -> Left err
  Right (_, 0, v) -> Right v
  _ -> Left "Not all bytes consumed"


