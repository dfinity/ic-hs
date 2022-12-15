module IC.Test.Options where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text as T
import Data.Proxy
import Data.List
import qualified Data.Word as W
import Test.Tasty.Options
import Options.Applicative hiding (str)
import Codec.Candid (Principal(..), parsePrincipal)
import IC.Crypto
import IC.Id.Forms(mkSelfAuthenticatingId)
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

defaultTestSubnetConfig :: TestSubnetConfig
defaultTestSubnetConfig = (getSubnetIdFromNonce "sk2", Application, 1)

getSubnetIdFromNonce :: String -> EntityId
getSubnetIdFromNonce nonce = EntityId $ mkSelfAuthenticatingId $ toPublicKey $ createSecretKeyBLS $ BLU.fromString nonce

newtype MyTestSubnet = MyTestSubnet TestSubnetConfig

instance Read MyTestSubnet where
  readsPrec p x = do
    ((p, t, r), z) <- (readsPrec p x :: [((String, SubnetType, W.Word64), String)])
    Principal b <- case parsePrincipal (T.pack p) of Left err -> error err
                                                     Right p -> return p
    return (MyTestSubnet (EntityId b, t, r), z)

instance Show MyTestSubnet where
  show (MyTestSubnet sub) = show sub

instance IsOption MyTestSubnet where
  defaultValue = MyTestSubnet defaultTestSubnetConfig
  parseValue = Just <$> read
  optionName = return "test-subnet-config"
  optionHelp = return $ "Test subnet configuration consisting of subnet ID, subnet type, and replication factor (default: " ++ show defaultTestSubnetConfig ++ ")"

myTestSubnetOption :: OptionDescription
myTestSubnetOption = Option (Proxy :: Proxy MyTestSubnet)

newtype OtherTestSubnet = OtherTestSubnet TestSubnetConfig

instance Read OtherTestSubnet where
  readsPrec p x = do
    ((p, t, r), z) <- (readsPrec p x :: [((String, SubnetType, W.Word64), String)])
    Principal b <- case parsePrincipal (T.pack p) of Left err -> error err
                                                     Right p -> return p
    return (OtherTestSubnet (EntityId b, t, r), z)

instance Show OtherTestSubnet where
  show (OtherTestSubnet sub) = show sub

instance IsOption OtherTestSubnet where
  defaultValue = OtherTestSubnet defaultTestSubnetConfig
  parseValue = Just <$> read
  optionName = return "test-subnet-config"
  optionHelp = return $ "Test subnet configuration consisting of subnet ID, subnet type, and replication factor (default: " ++ show defaultTestSubnetConfig ++ ")"

otherTestSubnetOption :: OptionDescription
otherTestSubnetOption = Option (Proxy :: Proxy OtherTestSubnet)
