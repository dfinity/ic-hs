{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Options (lookupOption)
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Ingredients.Rerun
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
    ac <- preFlight os
    let TestSubnetType subnet = lookupOption os
    defaultMainWithIngredients ingredients (icTests subnet ac)
  where
    ingredients =
      [ rerunningTests
        [ listingTests
        , includingOptions [endpointOption]
        , includingOptions [ecidOption]
        , includingOptions [httpbinOption]
        , includingOptions [polltimeoutOption]
        , includingOptions [subnettypeOption]
        , antXMLRunner `composeReporters` htmlRunner `composeReporters` consoleTestReporter
        ]
      ]
