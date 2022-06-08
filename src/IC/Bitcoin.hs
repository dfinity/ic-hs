{-# LANGUAGE OverloadedStrings #-}

module IC.Bitcoin where

import Data.Text
import Data.Word
import Numeric.Natural

import qualified Haskoin.Block.Common as B

import IC.Types

data State = State
  { stateMainnet :: MainnetState
  , stateTestnet :: TestnetState
  } deriving (Eq, Show)

newtype MainnetState = MainnetState { unMainnetState :: Blockchain } deriving (Eq, Show)
newtype TestnetState = TestnetState { unTestnetState :: Blockchain } deriving (Eq, Show)

newtype Blockchain = Blockchain { unBlockchain :: [B.Block] } deriving (Eq, Show)

-- TODO: init state properly
initState :: State
initState = State
  { stateMainnet = MainnetState $ Blockchain []
  , stateTestnet = TestnetState $ Blockchain []
}
