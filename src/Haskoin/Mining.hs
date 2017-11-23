module Haskoin.Mining where

import Data.List
import Haskoin.Types
import Haskoin.Serialization
import Crypto.Hash(hashlazy)
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Crypto.Number.Serialize(os2ip)
import Debug.Trace(trace)

type TransactionPool = IO [Transaction]

blockReward                    = 1000
globalTransactionLimit         = 1000
numBlocksToCalculateDifficulty = 4
genesisBlockDifficulty         = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
targetTime                     = 2


mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mineOn pendingTransactions minerAccount parent = do
  ts <- pendingTransactions
  ts <- return $ validTransactions parent ts
  ts <- return $ take globalTransactionLimit ts
  mine ts 0
  where
    validChain bc = difficulty bc < desiredDifficulty bc
    mine ts nonce = do
      now <- getPOSIXTime
      let header = BlockHeader {
            _minerAccount = minerAccount,
            _parentHash = hashlazy $ serialize parent,
            _nonce = nonce,
            _minedAt = now
          }
          block = Block ts
          candidate = Node block header parent
      if validChain candidate
         then return candidate
         else mine ts (nonce + 1)

difficulty :: Blockchain -> Integer
difficulty bc = os2ip $ (hashlazy $ serialize bc :: HaskoinHash)

blockTimeAverage :: Blockchain -> NominalDiffTime
blockTimeAverage bc = average $ zipWith (-) times (theTail times)
  where
    times = take numBlocksToCalculateDifficulty $ map _minedAt $ headers bc
    theTail xs
      | null xs = []
      | otherwise = tail xs

average :: (Foldable f, Num a, Fractional a, Eq a) => f a -> a
average xs = sum xs / (if d == 0 then 1 else d) where d = fromIntegral $ length xs


safeDiv n d = n / (if d == 0 then 1 else d)

desiredDifficulty :: Blockchain -> Integer
desiredDifficulty x = round $ loop x
  where
    loop (Genesis _)     = genesisBlockDifficulty
    loop x@(Node _ _ xs) = oldDifficulty / adjustmentFactor
      where
        oldDifficulty    = loop xs
        adjustmentFactor = min 4.0 $ targetTime `safeDiv` blockTimeAverage x


makeGenesis :: IO Blockchain
makeGenesis = return $ Genesis (Block [])


balances :: Blockchain -> M.Map Account Integer
balances bc =
  let txns = transactions (mconcat $ blocks bc)
      debits = map (\(Transaction from _ amount) -> (from, - amount)) txns
      credits = map (\(Transaction from _ amount) -> (from, amount)) txns
      minings = map (\h -> (_minerAccount h, blockReward)) $ headers bc
  in  M.fromListWith (+) $ debits ++ credits ++ minings


validTransactions :: Blockchain -> [Transaction] -> [Transaction]
validTransactions bc txns =
  let accounts = balances bc
      validTxn txn = case M.lookup (_from txn) accounts of
                       Nothing -> False
                       Just balance -> balance >= _amount txn
  in filter validTxn txns
