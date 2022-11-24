{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- |
Generic utilities related to standard or imported data structures that we do
don’t want to see in non-plumbing code.
-}
module IC.Utils where

import qualified Codec.Candid as Candid
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BS
import qualified Data.Row as R
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vec
import qualified Data.Word as W
import qualified Data.X509 as C
import Data.Row ((.!), type (.!))

import IC.Constants
import IC.Management

freshKey :: M.Map Int a -> Int
freshKey m | M.null m = 0
           | otherwise = fst (M.findMax m) + 1

repeatWhileTrue :: Monad m => m Bool -> m ()
repeatWhileTrue act = act >>= \case
    True -> repeatWhileTrue act
    False -> return ()


duplicates :: Ord a => [a] -> [a]
duplicates = go S.empty
  where
    go _s [] = []
    go s (x:xs) | x `S.member` s = x : go s' xs
                | otherwise      =     go s' xs
      where s' = S.insert x s


-- Wrappers to hide strict/lazy conversion from view
toUtf8 :: T.Text -> BS.ByteString
toUtf8 = BS.fromStrict . T.encodeUtf8

fromUtf8 :: BS.ByteString -> Maybe T.Text
fromUtf8 b = case T.decodeUtf8' (BS.toStrict b) of
    Left _ -> Nothing
    Right t -> Just t

-- Compute UTF-8 length of Text
utf8_length :: T.Text -> W.Word64
utf8_length = fromIntegral . BS.length . toUtf8

-- ic-ref config
data RefConfig = RefConfig
    { tc_root_certs :: [C.SignedCertificate]
    }

makeRefConfig :: [C.SignedCertificate] -> IO RefConfig
makeRefConfig root_certs = do
    return RefConfig
        { tc_root_certs = root_certs
        }

type HasRefConfig = (?refConfig :: RefConfig)

withRefConfig :: RefConfig -> (forall. HasRefConfig => a) -> a
withRefConfig tc act = let ?refConfig = tc in act

refConfig :: HasRefConfig => RefConfig
refConfig = ?refConfig

getRootCerts :: HasRefConfig => [C.SignedCertificate]
getRootCerts = tc_root_certs refConfig

-- Canister http_request
max_response_size :: (r .! "max_response_bytes") ~ Maybe W.Word64 => R.Rec r -> W.Word64
max_response_size r = aux $ fmap fromIntegral $ r .! #max_response_bytes
  where
    aux Nothing = max_response_bytes_limit
    aux (Just w) = w

http_request_fee :: (a -> IO b) ~ (ICManagement IO .! "http_request") => a -> W.Word64 -> W.Word64 -> W.Word64
http_request_fee r base per_byte = base + per_byte * total_bytes
  where
    response_size_fee Nothing = max_inter_canister_payload_in_bytes
    response_size_fee (Just max_response_size) = max_response_size
    transform_fee Nothing = 0
    transform_fee (Just t) = dec_var (t .! #function) + (fromIntegral $ BS.length (t .! #context))
    dec_var (Candid.FuncRef _ t) = utf8_length t
    body_fee Nothing = 0
    body_fee (Just t) = BS.length t
    total_bytes = response_size_fee (fmap fromIntegral $ r .! #max_response_bytes)
      + (fromIntegral $ utf8_length $ r .! #url)
      + (fromIntegral $ sum $ map (\h -> utf8_length (h .! #name) + utf8_length (h .! #value)) $ Vec.toList $ r .! #headers)
      + (fromIntegral $ body_fee $ r .! #body)
      + (fromIntegral $ transform_fee $ r .! #transform)

http_response_size :: (a -> IO b) ~ (ICManagement IO .! "http_request") => b -> W.Word64
http_response_size r = header_size + body_size
  where
    header_size = fromIntegral $ sum $ map (\h -> utf8_length (h .! #name) + utf8_length (h .! #value)) $ Vec.toList $ r .! #headers
    body_size = fromIntegral (BS.length (r .! #body))
