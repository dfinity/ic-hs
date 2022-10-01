module IC.Test.Options where

import Data.Proxy
import Data.List
import Test.Tasty.Options
import Options.Applicative hiding (str)

-- Configuration: The URL of of the endpoint to test

newtype Endpoint = Endpoint String

instance IsOption Endpoint where
  defaultValue = Endpoint "http://0.0.0.0:8001"
  parseValue s = Just $ Endpoint base
    where base | "/" `isSuffixOf` s = init s
               | otherwise = s
  optionName = return "endpoint"
  optionHelp = return "Internet Computer endpoint to connect to (default: http://0.0.0.0:8001)"
  optionCLParser = mkOptionCLParser (metavar "URL")

newtype CanisterHttpRequestsEndpoint = CanisterHttpRequestsEndpoint String

instance IsOption CanisterHttpRequestsEndpoint where
  defaultValue = CanisterHttpRequestsEndpoint "http://localhost:80/"
  parseValue p = Just $ CanisterHttpRequestsEndpoint $ read p
  optionName = return "canister_http_requests_endpoint"
  optionHelp = return "Endpoint of test HTTP server (default: http://localhost:80/)"

newtype PollTimeout = PollTimeout Int

instance IsOption PollTimeout where
  defaultValue = PollTimeout 300
  parseValue p = Just $ PollTimeout $ read p
  optionName = return "poll_timeout"
  optionHelp = return "Timeout for request polling in seconds (default: 300)"

endpointOption :: OptionDescription
endpointOption = Option (Proxy :: Proxy Endpoint)

canisterhttprequestsendpointOption :: OptionDescription
canisterhttprequestsendpointOption = Option (Proxy :: Proxy CanisterHttpRequestsEndpoint)

polltimeoutOption :: OptionDescription
polltimeoutOption = Option (Proxy :: Proxy PollTimeout)
