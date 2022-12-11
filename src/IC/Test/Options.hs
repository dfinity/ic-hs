module IC.Test.Options where

import Data.Proxy
import Data.List
import qualified Data.Word as W
import Test.Tasty.Options
import Options.Applicative hiding (str)
import IC.Id.Fresh(wordToId)
import IC.Types

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

-- Configuration: Effective canister id for user requests to selected methods of the management canister

newtype ECID = ECID CanisterId

instance IsOption ECID where
  defaultValue = ECID $ wordToId 0
  parseValue = fmap ECID . parsePrettyID
  optionHelp = return $ "Effective canister id for user requests to selected methods of the management canister (default: " ++ prettyID (wordToId 0) ++ ")"
  optionName = return "ecid"

ecidOption :: OptionDescription
ecidOption = Option (Proxy :: Proxy ECID)

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

newtype TestSubnet = TestSubnet (SubnetType, W.Word64)

instance Read TestSubnet where
  readsPrec p x = do
    (y, z) <- (readsPrec p x :: [((SubnetType, W.Word64), String)])
    return (TestSubnet y, z)

instance Show TestSubnet where
  show (TestSubnet sub) = show sub

instance IsOption TestSubnet where
  defaultValue = TestSubnet (Application, 1)
  parseValue = read
  optionName = return "test-subnet-configuration"
  optionHelp = return $ "Test subnet configuration consisting of subnet type and replication factor (default: " ++ show (TestSubnet (Application, 1)) ++ ")"

testSubnetOption :: OptionDescription
testSubnetOption = Option (Proxy :: Proxy TestSubnet)
