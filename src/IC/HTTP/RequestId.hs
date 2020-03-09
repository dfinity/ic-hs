{-# LANGUAGE TypeApplications #-}
module IC.HTTP.RequestId (requestId) where

import Numeric.Natural
import IC.HTTP.GenR
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.List (sort)
import Crypto.Hash (hashlazy, SHA256)
import Data.ByteArray (convert)
import Wasm.Binary.LEB128
import Data.Binary.Put (runPut)

type RequestId = BS.ByteString

requestId :: GenR -> RequestId
requestId (GRec hm) = h $ BS.concat $ sort $ map encodeKV $ HM.toList hm
requestId _ = error "requestID: expected a record"

encodeKV :: (T.Text, GenR) -> BS.ByteString
encodeKV (k,v) = h (encodeText k) <> h (encodeVal v)

encodeVal :: GenR -> BS.ByteString
encodeVal (GBlob b) = b
encodeVal (GText t) = encodeText t
encodeVal (GNat n) = encodeNat n
encodeVal (GRec _) = error "requestID: Nested record"

encodeText :: T.Text -> BS.ByteString
encodeText = BS.fromStrict . T.encodeUtf8

encodeNat :: Natural -> BS.ByteString
encodeNat = runPut . putSLEB128

h :: BS.ByteString -> BS.ByteString
h = BS.fromStrict . convert . hashlazy @SHA256
