{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

module IC.Constants where

import Codec.Candid (Principal(..), parsePrincipal)
import Data.Either.Combinators(fromRight')
import qualified Data.Word as W
import qualified Data.Text as T
import Numeric.Natural

import IC.Types

cDEFAULT_PROVISIONAL_CYCLES_BALANCE :: Natural
cDEFAULT_PROVISIONAL_CYCLES_BALANCE = 100_000_000_000_000

cDEFAULT_PRINCIPAL_ZERO_CONTROLLERS :: Principal
cDEFAULT_PRINCIPAL_ZERO_CONTROLLERS = fromRight' $ parsePrincipal $ T.pack "zrl4w-cqaaa-nocon-troll-eraaa-d5qc"

cDEFAULT_PRINCIPAL_MULTIPLE_CONTROLLERS :: Principal
cDEFAULT_PRINCIPAL_MULTIPLE_CONTROLLERS = fromRight' $ parsePrincipal $ T.pack "ifxlm-aqaaa-multi-pleco-ntrol-lersa-h3ae"

-- Subnets

canister_ids_per_subnet :: W.Word64
canister_ids_per_subnet = 1_048_576

nth_canister_range :: W.Word64 -> (W.Word64, W.Word64)
nth_canister_range n = (n * canister_ids_per_subnet, (n + 1) * canister_ids_per_subnet - 1)

-- reference_subnet_size is used for scaling cycle cost
-- and must never be set to zero!
reference_subnet_size :: W.Word64
reference_subnet_size = 13

-- Canister http_request limits
max_request_bytes_limit :: W.Word64
max_request_bytes_limit = 2_000_000

max_response_bytes_limit :: W.Word64
max_response_bytes_limit = 2_000_000

max_http_request_url_length :: W.Word64
max_http_request_url_length = 8192

http_headers_max_number :: Int
http_headers_max_number = 64

http_headers_max_name_value_length :: W.Word64
http_headers_max_name_value_length = 8 * 1024 -- 8 KiB

http_headers_max_total_size :: W.Word64
http_headers_max_total_size = 48 * 1024 -- 48 KiB

getHttpRequestBaseFee :: SubnetType -> W.Word64
getHttpRequestBaseFee Application = 400000000
getHttpRequestBaseFee VerifiedApplication = 400000000
getHttpRequestBaseFee System = 0

getHttpRequestPerByteFee :: SubnetType -> W.Word64
getHttpRequestPerByteFee Application = 100000
getHttpRequestPerByteFee VerifiedApplication = 100000
getHttpRequestPerByteFee System = 0
