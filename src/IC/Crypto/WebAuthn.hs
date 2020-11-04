{-# LANGUAGE OverloadedStrings #-}
module IC.Crypto.WebAuthn
 ( init
 , SecretKey
 , createKey
 , toPublicKey
 , sign
 , verify
 ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO.Temp
import System.IO.Unsafe
import System.IO
import System.Exit
import System.Process.ByteString.Lazy
import Control.Monad
import Data.Hashable

newtype SecretKey = SecretKey BS.ByteString
  deriving Show

withSecretKey :: SecretKey -> (FilePath -> IO a) -> IO a
withSecretKey (SecretKey sk) act =
  withSystemTempFile "webauthn-seckey.pem" $ \fn h -> BS.hPut h sk >> hClose h >> act fn

withPublicKey :: BS.ByteString -> (FilePath -> IO a) -> IO a
withPublicKey pk act =
  withSystemTempFile "webauthn-pubkey.cose" $ \fn h -> BS.hPut h pk >> hClose h >> act fn

withSig :: BS.ByteString -> (FilePath -> IO a) -> IO a
withSig sig act =
  withSystemTempFile "webauthn-sig.cbor" $ \fn h -> BS.hPut h sig >> hClose h >> act fn

toPublicKey :: SecretKey -> BS.ByteString
toPublicKey sk = unsafePerformIO $ do
  (code, out, err) <- withSecretKey sk $ \fn ->
     readProcessWithExitCode "ic-webauthn-cli" [ "get_public_key", fn ] mempty
  unless (code == ExitSuccess) $
    die $ "ic-webauthn-cli create failed: " ++ show code ++ "\n" ++ BS.unpack err
  return out

createKey :: BS.ByteString -> SecretKey
createKey seed = unsafePerformIO $ do
  -- seed is actually hex, but hey, decimal is also hex
  (code, out, err) <- readProcessWithExitCode
    "ic-webauthn-cli" [ "create" ] (BS.pack $ show (abs (hash seed) + 1) ++ "\n")
  unless (code == ExitSuccess) $
    die $ "ic-webauthn-cli create failed: " ++ show code ++ "\n" ++ BS.unpack err
  return (SecretKey out)

sign :: SecretKey -> BS.ByteString -> IO BS.ByteString
sign sk msg = do
  (code, out, err) <- withSecretKey sk $ \fn ->
    readProcessWithExitCode "ic-webauthn-cli" [ "sign", fn ] msg
  unless (code == ExitSuccess) $
    die $ "ic-webauthn-cli sign failed: " ++ show code ++ "\n" ++ BS.unpack err
  return out

verify :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Bool
verify pk msg sig = unsafePerformIO $ do
  (code, _out, _err) <- withPublicKey pk $ \pkf -> withSig sig $ \sigf ->
    readProcessWithExitCode "ic-webauthn-cli" [ "verify", pkf, sigf ] msg
  if code == ExitSuccess
  then return True
  else return False
