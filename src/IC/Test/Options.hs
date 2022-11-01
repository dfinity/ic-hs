module IC.Test.Options where

import Data.Proxy
import Data.List
import Test.Tasty.Options
import Options.Applicative hiding (str)
import IC.Id.Fresh(wordToId)
import IC.Types

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

newtype ECID = ECID CanisterId

instance IsOption ECID where
  defaultValue = ECID $ wordToId 0
  parseValue = fmap ECID . parsePrettyID
  optionHelp = return $ "Effective canister id for canister creation user requests (default: " ++ prettyID (wordToId 0) ++ ")"
  optionName = return "ecid"

newtype Httpbin = Httpbin String

instance IsOption Httpbin where
  defaultValue = Httpbin "https://httpbin.org"
  parseValue = Just . Httpbin
  optionName = return "httpbin"
  optionHelp = return "httpbin endpoint (default: https://httpbin.org)"
  optionCLParser = mkOptionCLParser (metavar "URL")

newtype PollTimeout = PollTimeout Int

instance IsOption PollTimeout where
  defaultValue = PollTimeout 300
  parseValue p = Just $ PollTimeout $ read p
  optionName = return "poll_timeout"
  optionHelp = return "Timeout for request polling in seconds (default: 300)"

endpointOption :: OptionDescription
endpointOption = Option (Proxy :: Proxy Endpoint)

ecidOption :: OptionDescription
ecidOption = Option (Proxy :: Proxy ECID)

httpbinOption :: OptionDescription
httpbinOption = Option (Proxy :: Proxy Httpbin)

polltimeoutOption :: OptionDescription
polltimeoutOption = Option (Proxy :: Proxy PollTimeout)
