module IC.Crypto.DER (Suite(..), encode, decode) where

import qualified Data.ByteString.Lazy as BS
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.BitArray

import IC.Crypto.DER.Decode

data Suite = Ed25519 | WebAuthn | ECDSA deriving Show

webAuthnOID :: OID
webAuthnOID = [1,3,6,1,4,1,56387,1,1]

ed25519OID :: OID
ed25519OID = [1,3,101,112]

ecPublicKeyOID :: OID
ecPublicKeyOID =[1,2,840,10045,2,1]

secp256r1OID :: OID
secp256r1OID = [1,2,840,10045,3,1,7]

encode :: Suite -> BS.ByteString -> BS.ByteString
encode Ed25519  = encodeDER [ed25519OID]
encode WebAuthn = encodeDER [webAuthnOID]
encode ECDSA    = encodeDER [ecPublicKeyOID, secp256r1OID]

encodeDER :: [OID] -> BS.ByteString -> BS.ByteString
encodeDER oids pk = encodeASN1 DER $
    [ Start Sequence
    , Start Sequence
    ] ++
    [ OID oid | oid <- oids ] ++
    [ End Sequence
    , BitString (toBitArray (BS.toStrict pk) 0)
    , End Sequence
    ]

decode :: BS.ByteString -> Either String (Suite, BS.ByteString)
decode bs = case safeDecode bs of
  Left err -> Left $ "Could not decode DER: " ++ show err
  Right asn -> case asn of
      [  Start Sequence
        , Start Sequence
        , OID algo
        , End Sequence
        , BitString ba
        , End Sequence
        ]
        | algo == webAuthnOID
        -> Right (WebAuthn, BS.fromStrict (bitArrayGetData ba))
        | algo == ed25519OID
        -> Right (Ed25519, BS.fromStrict (bitArrayGetData ba))
        | otherwise
        -> Left $ "Unexpected cipher: algo = " ++ show algo
      [  Start Sequence
        , Start Sequence
        , OID algo
        , OID curve
        , End Sequence
        , BitString ba
        , End Sequence
        ]
        | algo == ecPublicKeyOID && curve == ecPublicKeyOID
        -> Right (ECDSA, BS.fromStrict (bitArrayGetData ba))
        | otherwise
        -> Left $ "Unexpected cipher: algo = " ++ show algo
      _ -> Left $ "Unexpected DER shape: " ++ show asn
