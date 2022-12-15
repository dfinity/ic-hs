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
    let MyTestSubnet my_sub = lookupOption os
    let OtherTestSubnet other_sub = lookupOption os
    defaultMainWithIngredients ingredients (icTests my_sub other_sub ac)
  where
    ingredients =
      [ rerunningTests
        [ listingTests
        , includingOptions [endpointOption]
        , includingOptions [ecidOption]
        , includingOptions [httpbinOption]
        , includingOptions [polltimeoutOption]
        , includingOptions [myTestSubnetOption]
        , includingOptions [otherTestSubnetOption]
        , antXMLRunner `composeReporters` htmlRunner `composeReporters` consoleTestReporter
        ]
      ]
