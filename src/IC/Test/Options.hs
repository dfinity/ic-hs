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

-- TestSubnetConfig: helper functions

getSubnetIdFromNonce :: String -> EntityId
getSubnetIdFromNonce nonce = EntityId $ mkSelfAuthenticatingId $ toPublicKey $ createSecretKeyBLS $ BLU.fromString nonce

defaultSysTestSubnetConfig :: TestSubnetConfig
defaultSysTestSubnetConfig = (getSubnetIdFromNonce "sk1", System, 1, [nth_canister_range 0])

defaultAppTestSubnetConfig :: TestSubnetConfig
defaultAppTestSubnetConfig = (getSubnetIdFromNonce "sk2", Application, 1, [nth_canister_range 1])

readTestSubnetConfig :: Int -> ReadS TestSubnetConfig
readTestSubnetConfig p x = do
    ((id, typ, size, ranges), z) <- (readsPrec p x :: [((String, SubnetType, W.Word64, [(W.Word64, W.Word64)]), String)])
    Principal b <- case parsePrincipal (T.pack id) of Left err -> error err
                                                      Right p -> return p
    return ((EntityId b, typ, size, ranges), z)

-- Configuration: Test subnet

newtype TestSubnet = TestSubnet TestSubnetConfig

instance Read TestSubnet where
  readsPrec p x = (\(y, z) -> (TestSubnet y, z)) <$> readTestSubnetConfig p x

instance Show TestSubnet where
  show (TestSubnet (id, typ, size, ranges)) = show (prettyID id, typ, size, ranges)

instance IsOption TestSubnet where
  defaultValue = TestSubnet defaultSysTestSubnetConfig
  parseValue = Just <$> read
  optionName = return "test-subnet-config"
  optionHelp = return $ "Test subnet configuration consisting of subnet ID, subnet type, replication factor, and canister ranges; default: " ++ show (TestSubnet defaultSysTestSubnetConfig)

testSubnetOption :: OptionDescription
testSubnetOption = Option (Proxy :: Proxy TestSubnet)

-- Configuration: Peer subnet

newtype PeerSubnet = PeerSubnet TestSubnetConfig

instance Read PeerSubnet where
  readsPrec p x = (\(y, z) -> (PeerSubnet y, z)) <$> readTestSubnetConfig p x

instance Show PeerSubnet where
  show (PeerSubnet (id, typ, size, ranges)) = show (prettyID id, typ, size, ranges)

instance IsOption PeerSubnet where
  defaultValue = PeerSubnet defaultAppTestSubnetConfig
  parseValue = Just <$> read
  optionName = return "peer-subnet-config"
  optionHelp = return $ "Peer subnet configuration consisting of subnet ID, subnet type, replication factor, and canister ranges; default: " ++ show (PeerSubnet defaultAppTestSubnetConfig)

peerSubnetOption :: OptionDescription
peerSubnetOption = Option (Proxy :: Proxy PeerSubnet)
