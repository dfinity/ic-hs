{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module IC.Certificate.CBOR (encodeCert, decodeCert) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Data.Bifunctor
import Codec.CBOR.Term
import Codec.CBOR.Write
import Codec.CBOR.Read

import IC.Certificate
import IC.CBORPatterns
import IC.HashTree
import IC.HashTree.CBOR

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
