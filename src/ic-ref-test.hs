{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Runners.Html
import Data.IORef
import Control.Exception
import System.Exit

import IC.Test.Options
import IC.Test.Spec

main :: IO ()
main = withTestSuitePrimer $ \primeTestSuite ->
    defaultMainWithIngredients ingredients (icTests primeTestSuite)
  where
    ingredients =
      [ rerunningTests
        [ listingTests
        , includingOptions [endpointOption]
        , htmlRunner `composeReporters` consoleTestReporter
        ]
      ]

-- | This helper function runs the main action, and passes a way to prime the
-- test suite. If this primer is _not_ executed, the program will always
-- return success (but still list failing tests)
withTestSuitePrimer :: (IO () -> IO ()) -> IO ()
withTestSuitePrimer main = do
    specCompliant <- newIORef False
    let primeTestSuite = writeIORef specCompliant True
    main primeTestSuite `catch`
        (\(e :: ExitCode) -> readIORef specCompliant >>= \case
            False -> throwIO ExitSuccess
            True -> throwIO e)
