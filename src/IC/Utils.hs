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
import IC.Types

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

http_request_fee :: (a -> IO b) ~ (ICManagement IO .! "http_request") => a -> (SubnetType, W.Word64) -> W.Word64
http_request_fee r (subnet_type, subnet_size) = (normalized_fee * subnet_size) `div` reference_subnet_size
  where
    base = getHttpRequestBaseFee subnet_type
    per_byte = getHttpRequestPerByteFee subnet_type
    response_size_fee Nothing = max_response_bytes_limit
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
    normalized_fee = base + per_byte * total_bytes

http_request_headers_total_size :: (a -> IO b) ~ (ICManagement IO .! "http_request") => Integral c => a -> c
http_request_headers_total_size r = fromIntegral $ sum $ map (\h -> utf8_length (h .! #name) + utf8_length (h .! #value)) $ Vec.toList $ r .! #headers

check_http_request_headers_number :: (a -> IO b) ~ (ICManagement IO .! "http_request") => a -> Bool
check_http_request_headers_number r = length (Vec.toList $ r .! #headers) <= http_headers_max_number

check_http_request_headers_name_length :: (a -> IO b) ~ (ICManagement IO .! "http_request") => a -> Bool
check_http_request_headers_name_length r = all (\h -> utf8_length (h .! #name) <= http_headers_max_name_value_length) (Vec.toList $ r .! #headers)

check_http_request_headers_value_length :: (a -> IO b) ~ (ICManagement IO .! "http_request") => a -> Bool
check_http_request_headers_value_length r = all (\h -> utf8_length (h .! #value) <= http_headers_max_name_value_length) (Vec.toList $ r .! #headers)

check_http_request_headers_total_size :: (a -> IO b) ~ (ICManagement IO .! "http_request") => a -> Bool
check_http_request_headers_total_size r = http_request_headers_total_size r <= http_headers_max_total_size

http_request_size :: (a -> IO b) ~ (ICManagement IO .! "http_request") => Integral c => a -> c
http_request_size r = http_request_headers_total_size r + body_size (r .! #body)
  where
    body_size Nothing = 0
    body_size (Just b) = fromIntegral $ BS.length b


http_response_headers :: (a -> IO b) ~ (ICManagement IO .! "http_request") => b -> [(T.Text, T.Text)]
http_response_headers r = map (\h -> (h .! #name, h .! #value)) $ Vec.toList $ r .! #headers

http_response_headers_total_size :: (a -> IO b) ~ (ICManagement IO .! "http_request") => Integral c => b -> c
http_response_headers_total_size r = fromIntegral $ sum $ map (\h -> utf8_length (h .! #name) + utf8_length (h .! #value)) $ Vec.toList $ r .! #headers

check_http_response_headers_number :: (a -> IO b) ~ (ICManagement IO .! "http_request") => b -> Bool
check_http_response_headers_number r = length (Vec.toList $ r .! #headers) <= http_headers_max_number

check_http_response_headers_name_length :: (a -> IO b) ~ (ICManagement IO .! "http_request") => b -> Bool
check_http_response_headers_name_length r = all (\h -> utf8_length (h .! #name) <= http_headers_max_name_value_length) (Vec.toList $ r .! #headers)

check_http_response_headers_value_length :: (a -> IO b) ~ (ICManagement IO .! "http_request") => b -> Bool
check_http_response_headers_value_length r = all (\h -> utf8_length (h .! #value) <= http_headers_max_name_value_length) (Vec.toList $ r .! #headers)

check_http_response_headers_total_size :: (a -> IO b) ~ (ICManagement IO .! "http_request") => b -> Bool
check_http_response_headers_total_size r = http_response_headers_total_size r <= http_headers_max_total_size

http_response_size :: (a -> IO b) ~ (ICManagement IO .! "http_request") => b -> W.Word64
http_response_size r = http_response_headers_total_size r + body_size
  where
    body_size = fromIntegral (BS.length (r .! #body))
