{-# LANGUAGE OverloadedStrings #-}

module IC.Bitcoin where

import Data.Text
import Data.Word
import Numeric.Natural

import IC.Types

data State = State
  { mainnet :: MainnetState
  , testnet :: TestnetState
  } deriving (Eq, Show)

newtype MainnetState = MainnetState { unMainnetState :: NetworkState } deriving (Eq, Show)
newtype TestnetState = TestnetState { unTestnetState :: NetworkState } deriving (Eq, Show)

data NetworkState = NetworkState { genesis_block :: Block } deriving (Eq, Show)

data Block = Block
  { block_height :: Int
  , block_hash :: Blob
  , block_transactions :: [Transaction]
  , block_successors :: [Block]
  } deriving (Eq, Show)

data Transaction = Transaction
  { txid :: Blob
  , inputs :: [TxIn]
  , outputs :: [TxOut]
  } deriving (Eq, Show)

data OutPoint = OutPoint
  { op_txid :: Blob
  , op_vout :: Word32
  } deriving (Eq, Ord, Show)

data TxIn = TxIn { outpoint :: OutPoint } deriving (Eq, Show)

data TxOut = TxOut 
  { value :: Natural
  -- address could be retrieved from a script associated with a txout
  , address :: Text
  } deriving (Eq, Show)

-- TODO: init state properly
initState :: State
initState = State {
  mainnet = MainnetState $ NetworkState { genesis_block = Block { block_height = 0, block_hash = "", block_transactions = [], block_successors = []}} ,
  testnet = TestnetState $ NetworkState { genesis_block = Block { block_height = 0, block_hash = "", block_transactions = [], block_successors = []}}
}
