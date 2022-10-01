{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Options
import Test.Tasty.Runners.AntXML
import Test.Tasty.Runners.Html
import Test.Tasty.Runners

import IC.Test.Options
import IC.Test.Agent (preFlight)
import IC.Test.Spec
import qualified IC.Crypto.BLS as BLS

main :: IO ()
main = do
    BLS.init
    os <- parseOptions ingredients (testGroup "dummy" [])
    let TestPort tp = lookupOption os
    startTestServer tp
    ac <- preFlight os
    defaultMainWithIngredients ingredients (icTests ac)
  where
    ingredients =
      [ rerunningTests
        [ listingTests
        , includingOptions [endpointOption]
        , includingOptions [testportOption]
        , includingOptions [polltimeoutOption]
        , antXMLRunner `composeReporters` htmlRunner `composeReporters` consoleTestReporter
        ]
      ]
    startTestServer port = void $ forkIO $ run port app
      where app _ f = f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"
