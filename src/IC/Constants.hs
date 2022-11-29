{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}

module IC.Constants where

import qualified Data.Word as W
import Numeric.Natural

import IC.Types

cDEFAULT_PROVISIONAL_CYCLES_BALANCE :: Natural
cDEFAULT_PROVISIONAL_CYCLES_BALANCE = 100_000_000_000_000

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
