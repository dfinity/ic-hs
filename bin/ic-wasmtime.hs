{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
import Options.Applicative
import Data.Foldable
import Data.Maybe(fromJust)
import Data.Word as W
import qualified Data.X509.CertificateStore as C
import Control.Concurrent
import Control.Monad (join, forever)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp
import qualified Data.Text as T
import IC.Constants
import IC.HTTP
import IC.Types
import IC.Utils
import IC.Version
import qualified IC.Crypto.BLS as BLS
import Wasmtime
import qualified Data.ByteString.Lazy as BS
import Data.Vector as V
import Control.Monad.ST
import Control.Monad.Except
import UnliftIO.Exception
import Control.Concurrent
import System.Timeout
import IC.Wasm.Wasmtime

work :: IO ()
work = do
  wasm_mod <- BS.readFile "/home/martin/counter/c.wasm"
  inst <- runExceptT $ initialize wasm_mod --(systemAPI esref)
  putStrLn "ok"
  case inst of
    Left e -> putStrLn e
    Right r -> return ()
  putStrLn "ok"

main :: IO ()
main = do
  putStrLn "ok"
  work
  --threadDelay 1000000
