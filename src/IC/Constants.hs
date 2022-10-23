{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

module IC.Constants where

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Candid as Candid
import qualified Data.Row as R
import qualified Data.Row.Variants as V
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Data.Word as W
import Data.Row ((.!), type (.!))
import Numeric.Natural

import IC.Management
import IC.Types

cDEFAULT_PROVISIONAL_CYCLES_BALANCE :: Natural
cDEFAULT_PROVISIONAL_CYCLES_BALANCE = 100_000_000_000_000

-- Canister http_request limits
max_inter_canister_payload_in_bytes :: W.Word64
max_inter_canister_payload_in_bytes = 2 * 1024 * 1024

canister_http_response_limit :: Natural
canister_http_response_limit = 2 * 1024 * 1024 - 50 * 1024

max_http_request_url_length :: Int
max_http_request_url_length = 65534

-- Canister http_request fees
max_response_size :: (r .! "max_response_bytes") ~ Maybe W.Word64 => R.Rec r -> W.Word64
max_response_size r = aux $ fmap fromIntegral $ r .! #max_response_bytes
  where
    aux Nothing = max_inter_canister_payload_in_bytes
    aux (Just w) = w

http_request_fee :: (a -> IO b) ~ (ICManagement IO .! "http_request") => a -> W.Word64 -> W.Word64 -> W.Word64
http_request_fee r base per_byte = base + per_byte * total_bytes
  where
    response_size_fee Nothing = max_inter_canister_payload_in_bytes
    response_size_fee (Just max_response_size) = max_response_size
    transform_fee Nothing = 0
    transform_fee (Just v) = dec_var (V.trial' v #function)
    dec_var Nothing = error "transform variant in http_request must be #function"
    dec_var (Just (Candid.FuncRef _ t)) = T.length t
    body_fee Nothing = 0
    body_fee (Just t) = BS.length t
    total_bytes = response_size_fee (fmap fromIntegral $ r .! #max_response_bytes)
      + (fromIntegral $ T.length $ r .! #url)
      + (fromIntegral $ sum $ map (\h -> T.length (h .! #name) + T.length (h .! #value)) $ Vec.toList $ r .! #headers)
      + (fromIntegral $ body_fee $ r .! #body)
      + (fromIntegral $ transform_fee $ r .! #transform)

getHttpRequestBaseFee :: SubnetType -> W.Word64
getHttpRequestBaseFee Application = 400000000
getHttpRequestBaseFee VerifiedApplication = 400000000
getHttpRequestBaseFee System = 0

getHttpRequestPerByteFee :: SubnetType -> W.Word64
getHttpRequestPerByteFee Application = 100000
getHttpRequestPerByteFee VerifiedApplication = 100000
getHttpRequestPerByteFee System = 0
