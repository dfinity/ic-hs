-- Unit tests for IC.Canister.StableMemory
{-# LANGUAGE OverloadedStrings #-}
module IC.Test.StableMemory (stableMemoryTests) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Except
import Control.Monad.ST

import qualified IC.Canister.StableMemory as Stable

runHostM :: ExceptT String IO a -> IO (Either String a)
runHostM = runExceptT

mkMem :: Int -> ExceptT String IO (Stable.Memory)
mkMem numPages = do
  mem <- lift $ Stable.new
  r <- lift $ Stable.grow mem (fromIntegral numPages)
  when (r < 0) $
    throwError "grow failed"
  return mem

stableMemoryTests :: TestTree
stableMemoryTests = testGroup "Stable memory tests"
  [ testCase "grow" $ do
      res <- runHostM $ do mem <- lift $ Stable.new
                           old <- lift $ Stable.grow mem 5
                           new <- lift $ Stable.size mem
                           return (old, new)
      res @?= Right (0, 5)
  , testCase "read across multiple writes" $ do
      res <- runHostM $ do mem <- mkMem 1
                           Stable.write mem 0 "abc"
                           Stable.write mem 6 "efg"
                           Stable.read mem 0 9
      res @?= Right "abc\NUL\NUL\NULefg"
  , testCase "read part of a single write" $ do
      res <- runHostM $ do mem <- mkMem 1
                           Stable.write mem 0 "abcdefghijk"
                           Stable.read mem 3 3
      res @?= Right "def"
  , testCase "read overlapping writes" $ do
      res <- runHostM $ do mem <- mkMem 1
                           Stable.write mem 0 "abcdef"
                           Stable.write mem 3 "xyz123"
                           Stable.read mem 3 3
      res @?= Right "xyz"
  , testCase "read several overlapping writes" $ do
      res <- runHostM $ do mem <- mkMem 1
                           Stable.write mem 0 "abcdefghij"
                           Stable.write mem 1 "zw"
                           Stable.write mem 6 "xyz"
                           Stable.read mem 0 11
      res @?= Right "azwdefxyzj\NUL"
  , testCase "export/import" $ do
      res <- runHostM $ do mem <- mkMem 2
                           Stable.write mem 0 "ABCD"
                           blob <- lift $ Stable.serialize <$> Stable.export mem
                           mem2 <- lift $ Stable.new
                           lift $ Stable.imp mem2 $ Stable.deserialize blob
                           size2 <- lift $ Stable.size mem2
                           data2 <- Stable.read mem2 0 6
                           return (size2, data2)
      res @?= Right (2, "ABCD\NUL\NUL")
  , testCase "read out of bounds" $ do
      res <- runHostM $ do mem <- mkMem 1
                           Stable.read mem 0 70000
      case res of
        Left _ -> return ()
        Right _ -> assertFailure "reading out of bounds is an error"
  ]
