module IC.Test.Options where

import Data.Proxy
import Data.List
import Test.Tasty.Options
import Options.Applicative hiding (str)

-- Configuration: The URL of the endpoint to test

newtype Endpoint = Endpoint String

instance IsOption Endpoint where
  defaultValue = Endpoint "http://0.0.0.0:8001"
  parseValue s = Just $ Endpoint base
    where base | "/" `isSuffixOf` s = init s
               | otherwise = s
  optionName = return "endpoint"
  optionHelp = return "Internet Computer endpoint to connect to (default: http://0.0.0.0:8001)"
  optionCLParser = mkOptionCLParser (metavar "URL")

endpointOption :: OptionDescription
endpointOption = Option (Proxy :: Proxy Endpoint)

-- Configuration: The URL of the httpbin endpoint for http_request tests

newtype Httpbin = Httpbin String

instance IsOption Httpbin where
  defaultValue = Httpbin "httpbin.org"
  parseValue = Just . Httpbin
  optionName = return "httpbin"
  optionHelp = return "httpbin endpoint (default: httpbin.org)"
  optionCLParser = mkOptionCLParser (metavar "URL")

httpbinOption :: OptionDescription
httpbinOption = Option (Proxy :: Proxy Httpbin)

-- Configuration: Timeout for polling on the status of asynchronous requests

newtype PollTimeout = PollTimeout Int

instance IsOption PollTimeout where
  defaultValue = PollTimeout 300
  parseValue p = Just $ PollTimeout $ read p
  optionName = return "poll-timeout"
  optionHelp = return "Timeout for request polling in seconds (default: 300)"

polltimeoutOption :: OptionDescription
polltimeoutOption = Option (Proxy :: Proxy PollTimeout)

-- Configuration: Subnet type

data SubnetType = Application | VerifiedApplication | System

instance Read SubnetType where
  readsPrec _ x = do
    if x == "application" then return (Application, "")
    else if x == "verified_application" then return (VerifiedApplication, "")
    else if x == "system" then return (System, "")
    else fail "could not read SubnetType"

instance Show SubnetType where
  show Application = "application"
  show VerifiedApplication = "verified_application"
  show System = "system"

instance IsOption SubnetType where
  defaultValue = Application
  parseValue p
    | p == "application" = Just Application
    | p == "verified_application" = Just VerifiedApplication
    | p == "system" = Just System
    | otherwise = Nothing
  optionName = return "subnet-type"
  optionHelp = return "Subnet type [possible values: application, verified_application, system] (default: application)"

subnettypeOption :: OptionDescription
subnettypeOption = Option (Proxy :: Proxy SubnetType)
