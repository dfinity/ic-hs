{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Runners.Html
import Test.Tasty.Runners
import Control.Exception
import System.Exit

import IC.Test.Options
import IC.Test.Spec

main :: IO ()
main = do
    os <- parseOptions ingredients (testGroup "dummy" [])
    tc <- preFlight os
    defaultMainWithIngredients ingredients (icTests tc)
      `catch`
        -- If not primed, always succeed
        (\(e :: ExitCode) -> if tc_primed tc then throwIO e else do
            putStrLn "Test suite not primed, returning success despite failures."
            throwIO ExitSuccess)
  where
    ingredients =
      [ rerunningTests
        [ listingTests
        , includingOptions [endpointOption]
        , htmlRunner `composeReporters` consoleTestReporter
        ]
      ]
