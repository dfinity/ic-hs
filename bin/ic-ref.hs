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

defaultPort :: Port
defaultPort = 8001


work :: [(SubnetType, W.Word64, String, [(W.Word64, W.Word64)])] -> Maybe String -> Int -> Maybe Int -> Maybe FilePath -> Maybe FilePath -> Bool ->  IO ()
work subnets maybe_cert_path systemTaskPeriod portToUse writePortTo backingFile log = do
    let subs = map (\(t, n, nonce, ranges) -> SubnetConfig t n nonce ranges) subnets
    putStrLn "Starting ic-ref..."
    BLS.init
    certs <- case maybe_cert_path of Nothing -> return []
                                     Just ps -> C.listCertificates <$> fromJust <$> C.readCertificateStore ps
    conf <- makeRefConfig certs
    withRefConfig conf $ withApp subs (systemTaskPeriod * 1000000) backingFile $ \app -> do
        let app' =  laxCorsSettings $ if log then logStdoutDev app else app
        case portToUse of
          Nothing ->
            withApplicationSettings settings (pure app') $ \port -> do
              greet port
              forever (threadDelay maxBound)
          Just port -> do
            greet port
            runSettings (setPort port settings) app'
  where
    greet port = do
       putStrLn $ "Running at http://127.0.0.1:" ++ show port ++ "/"
       for_ writePortTo $ \fn -> writeFile fn (show port)

    settings = setHost "127.0.0.1" defaultSettings

    -- Make sure that we reply succesfully to preflight checks.
    laxCorsSettings = cors $ \_ ->
        Just simpleCorsResourcePolicy
          { corsOrigins = Nothing,
            corsMethods = [ "GET" ],
            corsRequestHeaders = simpleHeaders ++ [ "X-Requested-With" ]
          }

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> versions <*> parser)
  (  fullDesc
  <> header ("Internet Computer reference implementation " <> T.unpack implVersion)
  <> progDesc (
    "A stand-alone local reference implementation of the Internet Computer. \
    \By default, it listens on http://127.0.0.1:" ++ show defaultPort ++ "/. You \
    \can change the port with --pick-port or --listen-port.")
  )
  where
    versions :: Parser (a -> a)
    versions =
          infoOption (T.unpack implVersion) (long "version" <> help "show version number")
      <*> infoOption (T.unpack specVersion) (long "spec-version" <> help "show spec version number")
    defaultSubnetConfig :: [(SubnetType, W.Word64, String, [(W.Word64, W.Word64)])]
    defaultSubnetConfig = [(System, 1, "sk1", [nth_canister_range 0]), (Application, 1, "sk2", [nth_canister_range 1])]
    defaultSystemTaskPeriod :: Int
    defaultSystemTaskPeriod = 1
    parser :: Parser (IO ())
    parser = work
      <$>
        (
          (
            option auto
            (  long "subnet-config"
            <> help ("choose initial subnet configuration consisting of subnet type, replication factor, nonce, and canister ranges for every subnet (default: " ++ show defaultSubnetConfig ++ ")")
            )
          )
        <|> pure defaultSubnetConfig
        )
      <*> optional (strOption
            (  long "cert-path"
            <> help "path to certificate file or directory"
            )
          )
      <*>
        (
          (
            option auto
            (  long "system-task-period"
            <> help ("choose execution period (in integer seconds) for system tasks, i.e., heartbeats and global timers (default: " ++ show defaultSystemTaskPeriod ++ ")")
            )
          )
        <|> pure defaultSystemTaskPeriod
        )
      <*>
        ( flag' Nothing
          (  long "pick-port"
          <> help ("pick a free port (instead of binding to 127.0.0.1:" ++ show defaultPort ++ ")")
          )
        <|>
          (Just <$>
            option auto
            (  long "listen-port"
            <> help "specify the listen port"
            )
          )
        <|> pure (Just defaultPort)
        )
      <*> optional (strOption
          (  long "write-port-to"
          <> help "write port to the given file"
        ))
      <*> optional (strOption
          (  long "state-file"
          <> metavar "FILE"
          <> help "file to persist IC state in"
        ))
      <*> switch
          (  long "http-log"
          <> help "print a HTTP log to stdout"
          )
