-- Unit/Prop tests for IC.HashTree
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IC.Test.HashTree (hashTreeTests) where

import Data.List
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Map.Lazy as M
import qualified Data.ByteString.Lazy as BS
import IC.HashTree
import Data.Bifunctor

hashTreeTests :: TestTree
hashTreeTests = testGroup "Hash tree tests"
  [ testProperty "lookup succeeds" $ \lt (AsPath p)->
    lookupPath (construct lt) p === lookupL lt p
  , testProperty "prune preserves hash" $ \lt (AsPaths ps) ->
    let ht = construct lt
    in reconstruct ht === reconstruct (prune ht ps)
  , testProperty "prune preserves lookups" $ \lt (AsPaths ps) (AsPath p) ->
    let ht = construct lt
    in notError (lookupPath ht p) ==>
        if any (`isPrefixOf` p) ps
        then lookupPath (prune ht ps) p === lookupPath ht p
        else lookupPath (prune ht ps) p `elemP` [Unknown, Absent]
  ]


-- This is, in a way, the spec for lookupPath
lookupL :: LabeledTree -> Path -> Res
lookupL (Value _) (_:_) = Error "Found leaf when expecting subtree"
lookupL (SubTrees sts) (l:ls) = case M.lookup l sts of
    Just st -> lookupL st ls
    Nothing -> Absent
lookupL (Value v) [] = Found v
lookupL (SubTrees _) [] = Error "Found forest when expecting leaf"

notError :: Res -> Bool
notError (Error _) = False
notError _ = True

-- Property based testing infrastructure
-- (slightly more verbose because IC.HashTree is not very typed

elemP :: (Eq a, Show a) => a -> [a] -> Property
x `elemP` xs = disjoin $ map (x ===) xs

genValue :: Gen Value
genValue = BS.pack <$> arbitrary

genLabel :: Gen Label
genLabel = oneof [ pure "", pure "hello", pure "world", BS.pack <$> arbitrary ]


newtype AsLabel = AsLabel { asLabel :: Label }
instance Arbitrary AsLabel where arbitrary = AsLabel <$> genLabel
instance Show AsLabel where show (AsLabel l) = show l

newtype AsPath = AsPath { asPath :: Path }
instance Arbitrary AsPath where
    arbitrary = AsPath . map asLabel <$> arbitrary
    shrink (AsPath ps) = map AsPath (init (inits ps))
instance Show AsPath where show (AsPath l) = show l

newtype AsPaths = AsPaths { _asPaths :: [Path] }
instance Arbitrary AsPaths where
    arbitrary = AsPaths . map asPath <$> arbitrary
    shrink (AsPaths ps) = AsPaths <$>
        [ as ++ bs         | (as,_,bs) <- splits ] ++
        [ as ++ [v'] ++ bs | (as,v,bs) <- splits, AsPath v' <- shrink (AsPath v) ]
      where
        splits = [(as,v,bs) | i <- [0..length ps-1], (as,v:bs) <- pure $ splitAt i ps ]
instance Show AsPaths where show (AsPaths l) = show l

instance Arbitrary LabeledTree where
    arbitrary = sized go
      where
        go 0 = Value <$> genValue
        go n = oneof
            [ Value <$> genValue
            , resize (n `div` 2) $
                SubTrees . M.fromList . map (first asLabel) <$> arbitrary
            ]
    shrink (Value _) = [Value ""]
    shrink (SubTrees m) = SubTrees <$>
        [ M.delete k m | k <- M.keys m ] ++
        [ M.insert k v' m | (k,v) <- M.toList m, v' <- shrink v ]


