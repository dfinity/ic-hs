module IC.CBOR.Utils where

import qualified Data.ByteString.Builder as BS
import IC.HTTP.CBOR
import IC.HTTP.GenR
import IC.Types

encodePrincipalList :: [EntityId] -> Blob
encodePrincipalList entities = BS.toLazyByteString $ encode $ GList $ map (GBlob . rawEntityId) entities

encodeCanisterRangeList :: [CanisterRange] -> Blob
encodeCanisterRangeList ranges = BS.toLazyByteString $ encode $ GList $ map (\r -> GList $ map (GBlob . rawEntityId) [fst r, snd r]) ranges