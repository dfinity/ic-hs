module IC.Test.Options where

import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Text as T
import Data.Proxy
import Data.List
import qualified Data.Word as W
import Test.Tasty.Options
import Options.Applicative hiding (str)
import Codec.Candid (Principal(..), parsePrincipal)
import IC.Constants
import IC.Crypto
import IC.Id.Forms(mkSelfAuthenticatingId)
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

-- Configuration: Test subnet

getSubnetIdFromNonce :: String -> EntityId
getSubnetIdFromNonce nonce = EntityId $ mkSelfAuthenticatingId $ toPublicKey $ createSecretKeyBLS $ BLU.fromString nonce

defaultSysTestSubnetConfig :: TestSubnetConfig
defaultSysTestSubnetConfig = (getSubnetIdFromNonce "sk1", System, 1, [nth_canister_range 0])

defaultAppTestSubnetConfig :: TestSubnetConfig
defaultAppTestSubnetConfig = (getSubnetIdFromNonce "sk2", Application, 1, [nth_canister_range 1])

newtype MyTestSubnet = MyTestSubnet TestSubnetConfig

instance Read MyTestSubnet where
  readsPrec p x = do
    ((id, typ, size, ranges), z) <- (readsPrec p x :: [((String, SubnetType, W.Word64, [(W.Word64, W.Word64)]), String)])
    Principal b <- case parsePrincipal (T.pack id) of Left err -> error err
                                                      Right p -> return p
    return (MyTestSubnet (EntityId b, typ, size, ranges), z)

instance Show MyTestSubnet where
  show (MyTestSubnet (id, typ, size, ranges)) = show (prettyID id, typ, size, ranges)

instance IsOption MyTestSubnet where
  defaultValue = MyTestSubnet defaultSysTestSubnetConfig
  parseValue = Just <$> read
  optionName = return "test-subnet-config"
  optionHelp = return $ "Test subnet configuration consisting of subnet ID, subnet type, replication factor, and canister ranges; default sys: " ++ show (MyTestSubnet defaultSysTestSubnetConfig) ++ "; default app: " ++ show (MyTestSubnet defaultAppTestSubnetConfig)

myTestSubnetOption :: OptionDescription
myTestSubnetOption = Option (Proxy :: Proxy MyTestSubnet)

-- Configuration: Peer subnet

newtype OtherTestSubnet = OtherTestSubnet TestSubnetConfig

instance Read OtherTestSubnet where
  readsPrec p x = do
    ((id, typ, size, ranges), z) <- (readsPrec p x :: [((String, SubnetType, W.Word64, [(W.Word64, W.Word64)]), String)])
    Principal b <- case parsePrincipal (T.pack id) of Left err -> error err
                                                      Right p -> return p
    return (OtherTestSubnet (EntityId b, typ, size, ranges), z)

instance Show OtherTestSubnet where
  show (OtherTestSubnet (id, typ, size, ranges)) = show (prettyID id, typ, size, ranges)

instance IsOption OtherTestSubnet where
  defaultValue = OtherTestSubnet defaultAppTestSubnetConfig
  parseValue = Just <$> read
  optionName = return "peer-test-subnet-config"
  optionHelp = return $ "Peer test subnet configuration consisting of subnet ID, subnet type, replication factor, and canister ranges; default app: " ++ show (OtherTestSubnet defaultAppTestSubnetConfig) ++ "; default sys: " ++ show (OtherTestSubnet defaultSysTestSubnetConfig)

otherTestSubnetOption :: OptionDescription
otherTestSubnetOption = Option (Proxy :: Proxy OtherTestSubnet)
