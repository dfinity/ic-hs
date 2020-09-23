{-# LANGUAGE OverloadedStrings #-}
module IC.Funds where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import Data.List
import Numeric.Natural

type Unit = BS.ByteString

cycle_unit, icpt_unit :: Unit
cycle_unit = "\x00"
icpt_unit = "\x01"

type Funds = M.Map Unit Natural

no_funds :: Funds
no_funds = mempty

le_funds :: Funds -> Funds -> Bool
f1 `le_funds` f2 = M.isSubmapOfBy (<=) (M.filter (>0) f1) f2

add_funds :: Funds -> Funds -> Funds
add_funds = M.unionWith (+)

sum_funds :: [Funds] -> Funds
sum_funds = foldl' add_funds no_funds

sub_funds :: Funds -> Funds -> Funds
sub_funds = M.differenceWith (\x y -> Just (x - y))

unit_funds :: Unit -> Natural -> Funds
unit_funds = M.singleton

cycle_funds :: Natural -> Funds
cycle_funds = unit_funds cycle_unit

icpt_funds :: Natural -> Funds
icpt_funds = unit_funds icpt_unit

(!$) :: Funds -> Unit -> Natural
f !$ u = M.findWithDefault 0 u f

