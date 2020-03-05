{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Control.Monad (join)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO as T
import qualified Text.Hex as H
import System.IO

import IC.HTTP.CBOR
import IC.HTTP.RequestId

work :: Maybe FilePath -> IO ()
work input = do
  request <- maybe BS.getContents BS.readFile input
  case decode request of
    Left err -> do
        T.hPutStrLn stderr "Failed to decode CBOR:"
        T.hPutStrLn stderr err
    Right gr ->
        T.putStrLn $ H.encodeHex $ BS.toStrict $ requestId gr

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "Internet Computer request id"
  <> progDesc "Given a CBOR-encoded request, calculate the request id"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> optional (strArgument
            (  metavar "FILE"
            <> help "file to read (default: stdin)"
            ))
