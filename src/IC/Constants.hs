{-# LANGUAGE NumericUnderscores #-}

module IC.Constants where

import qualified Data.Word as W
import Numeric.Natural

cDEFAULT_PROVISIONAL_CYCLES_BALANCE :: Natural
cDEFAULT_PROVISIONAL_CYCLES_BALANCE = 100_000_000_000_000

canister_ids_per_subnet :: W.Word64
canister_ids_per_subnet = 1_048_576
