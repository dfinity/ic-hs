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

newtype TestPort = TestPort Int

instance IsOption TestPort where
  defaultValue = TestPort 8003
  parseValue p = Just $ TestPort $ read p
  optionName = return "test_port"
  optionHelp = return "Port a test HTTP server listens on (default: 8003)"

endpointOption :: OptionDescription
endpointOption = Option (Proxy :: Proxy Endpoint)
