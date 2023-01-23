module IC.CBOR.Utils where

import qualified Data.ByteString.Builder as BS
import qualified Data.Text as T
import Codec.CBOR.Term
import IC.CBOR.Patterns
import IC.HTTP.CBOR
import IC.HTTP.GenR
import IC.Types

encodePrincipalList :: [EntityId] -> Blob
encodePrincipalList entities = BS.toLazyByteString $ encode $ GList $ map (GBlob . rawEntityId) entities

encodeCanisterRangeList :: [CanisterRange] -> Blob
encodeCanisterRangeList ranges = BS.toLazyByteString $ encode $ GList $ map (\(l, u) -> GList $ map (GBlob . rawEntityId) [l, u]) ranges

parseCanisterRanges :: Term -> Either T.Text [(Blob, Blob)]
parseCanisterRanges (TList_ ts) = case sequence (map go ts) of
   Nothing -> Left $ T.pack $ "Cannot parse as Canister Ranges: " ++ show ts
   Just bs -> Right bs
  where
    go (TList_ [ TBlob p1, TBlob p2 ]) = Just (p1, p2)
    go _ = Nothing
parseCanisterRanges t = Left $ T.pack $ "Cannot parse as Canister Ranges: " ++ show t
