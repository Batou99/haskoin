module Haskoin.Mining where

import Protolude
import Haskoin.Types
import Haskoin.Serialization
import Crypto.Hash(hashlazy)

type TransactionPool = IO [Transaction]

mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mineOn pendingTransactions minerAccount parent = do
  ts <- pendingTransactions
  let block = Block ts
  let header = BlockHeader {
      _miner = minerAccount,
      _parentHash = hashlazy $ serialize parent
      }
  return $ Node block header parent


makeGenesis :: IO Blockchain
makeGenesis = return $ Genesis (Block [])
