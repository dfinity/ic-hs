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
max_inter_canister_payload_in_bytes :: W.Word64
max_inter_canister_payload_in_bytes = 2 * 1024 * 1024

max_response_bytes_limit :: W.Word64
max_response_bytes_limit = 2000000

canister_http_response_limit :: Natural
canister_http_response_limit = 2000000

max_http_request_url_length :: W.Word64
max_http_request_url_length = 8192

getHttpRequestBaseFee :: SubnetType -> W.Word64
getHttpRequestBaseFee Application = 400000000
getHttpRequestBaseFee VerifiedApplication = 400000000
getHttpRequestBaseFee System = 0

getHttpRequestPerByteFee :: SubnetType -> W.Word64
getHttpRequestPerByteFee Application = 100000
getHttpRequestPerByteFee VerifiedApplication = 100000
getHttpRequestPerByteFee System = 0
