import Network.Wai.Handler.Warp
import IC.HTTP.WAI


main :: IO ()
main = do
    putStrLn "Starting ic-ref on http://0.0.0.0:8001/"
    app <- IC.HTTP.WAI.startApp
    run 8001 app
