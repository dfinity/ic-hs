import Options.Applicative
import Data.Foldable
import Control.Concurrent
import Control.Monad (join, forever)
import Network.Wai.Handler.Warp
import IC.HTTP.WAI

defaultPort :: Port
defaultPort = 8001


work :: Bool -> Maybe FilePath -> IO ()
work pickPort writePortTo = do
    putStrLn "Starting ic-ref..."
    if pickPort
    then withApplication IC.HTTP.WAI.startApp $ \port -> do
        greet port
        forever (threadDelay maxBound)
    else do
        app <- IC.HTTP.WAI.startApp
        greet defaultPort
        run defaultPort app
  where
    greet port = do
       putStrLn $ "Running at http://0.0.0.0:" ++ show port ++ "/"
       for_ writePortTo $ \fn -> writeFile fn (show port)


main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "Internet Computer reference implementation"
  <> progDesc "A stand-alone local reference implementation of the Internet Computer"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> switch
            (  long "pick-port"
            <> help ("pick a free port (instead of binding to " ++ show defaultPort ++ ")")
            )
        <*> optional (strOption
            (  long "write-port-to"
            <> help "write port to the given file"
            ))

