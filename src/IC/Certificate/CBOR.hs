{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module IC.Certificate.CBOR (encodeCert, decodeCert) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Numeric.Natural
import Data.Bifunctor
import Codec.CBOR.Term
import Codec.CBOR.Write
import Codec.CBOR.Read

import IC.Certificate
import IC.HashTree

encodeCert :: Certificate -> Blob
encodeCert Certificate{..} = toLazyByteString $ encodeTerm $ TTagged 55799 $ TMap $
    [ (TString "tree",  encodeHashTree cert_tree)
    , (TString "signature", TBlob cert_sig)
    ] ++
    [ (TString "delegation",  TMap
      [ (TString "subnet_id", TBlob del_subnet_id)
      , (TString "certificate", TBlob del_certificate)
      ])
    | Just Delegation{..} <- pure cert_delegation
    ]

encodeHashTree :: HashTree -> Term
encodeHashTree = go
  where
    go EmptyTree =     TList [ TInteger 0 ]
    go (Fork t1 t2) =  TList [ TInteger 1, go t1, go t2 ]
    go (Labeled l t) = TList [ TInteger 2, TBlob l, go t ]
    go (Leaf v) =      TList [ TInteger 3, TBlob v ]
    go (Pruned h) =    TList [ TInteger 4, TBlob h ]

decodeCert :: Blob -> Either String Certificate
decodeCert s =
    first (\(DeserialiseFailure _ s) -> "CBOR decoding failure: " <> s)
        (deserialiseFromBytes decodeTerm s) >>= begin
  where
    begin (leftOver, _) | not (BS.null leftOver) = Left "Left-over bytes"
    begin (_, TTagged 55799 t) = main t
    begin (_, t) = Left $ "Expected certificate to begin with tag 55799, got " ++ show t ++ " in " ++ show s

    main (TMap_ kv) = do
        cert_tree <- field "tree" kv >>= parseHashTree
        cert_sig <- field "signature" kv >>= parseBlob "signature"
        cert_delegation <- optionalField "delegation" kv >>= mapM parseDelegation
        return $ Certificate{..}
    main t = Left $ "expected certificate, found " ++ show t

parseBlob :: String -> Term -> Either String Blob
parseBlob _ (TBlob s) = return s
parseBlob what t = Left $ "expected " ++ what ++ ", found " ++ show t

parseHashTree :: Term -> Either String HashTree
parseHashTree = go
  where
    go (TList_ [ TNat 0 ]) = return EmptyTree
    go (TList_ [ TNat 1, t1, t2 ]) = Fork <$> parseHashTree t1 <*> parseHashTree t2
    go (TList_ [ TNat 2, TBlob l, t ]) = Labeled l <$> parseHashTree t
    go (TList_ [ TNat 3, TBlob v ]) = return $ Leaf v
    go (TList_ [ TNat 4, TBlob h ]) = return $ Pruned h
    go t = Left $ "Cannot parse as a Hash Tree: " ++ show t

parseDelegation :: Term -> Either String Delegation
parseDelegation (TMap_ kv) = do
    del_subnet_id <- field "subnet_id" kv >>= parseBlob "subnet_id"
    del_certificate <- field "certificate" kv >>= parseBlob "certificate"
    return $ Delegation{..}
parseDelegation t = Left $ "expected delegation, found " ++ show t

field :: T.Text -> [(Term, a)] -> Either String a
field f kv = case lookup (TString f) kv of
    Just t -> return t
    Nothing -> Left $ "Missing expected field " ++ show f

optionalField :: T.Text -> [(Term, a)] -> Either String (Maybe a)
optionalField f kv = return $ lookup (TString f) kv

pattern TMap_ :: [(Term, Term)] -> Term
pattern TMap_ m <- (\case {TMapI m -> Just m; TMap m -> Just m; _ -> Nothing} -> Just m)

pattern TList_ :: [Term] -> Term
pattern TList_ m <- (\case {TListI m -> Just m; TList m -> Just m; _ -> Nothing} -> Just m)

pattern TNat :: Natural -> Term
pattern TNat m <- (\case
        TInt m | m >= 0 -> Just (fromIntegral m)
        TInteger m | m >= 0 -> Just (fromIntegral m)
        _ -> Nothing
    -> Just m)


pattern TBlob :: Blob -> Term
pattern TBlob m <- TBytes (BS.fromStrict -> m)
  where TBlob m = TBytes (BS.toStrict m)




