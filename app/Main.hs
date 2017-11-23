module Main where

import Haskoin.Types
import Haskoin.Mining
import Data.Time.Clock
import Control.Monad
import Text.Pretty.Simple

testMining :: IO Blockchain
testMining = do
  chain <- makeGenesis
  chain <- mineOn (return [Transaction 0 1 100]) 0 chain
  chain <- mineOn (return [Transaction 0 2 100]) 0 chain
  chain <- mineOn (return [Transaction 0 3 100]) 0 chain
  chain <- mineOn (return [Transaction 0 4 100]) 0 chain
  chain <- mineOn (return [Transaction 0 1 100]) 0 chain
  return chain

timeForLastBlock :: Blockchain -> NominalDiffTime
timeForLastBlock bc =
  timeX - timeY
  where
    (x:y:_) = headers bc
    timeX = _minedAt x
    timeY = _minedAt y

runStep :: Blockchain -> IO Blockchain 
runStep chain = do
  chain <- mineOn (return [Transaction 1 5 10]) 1 chain
  pPrint $ "desired diff: " ++ (show $ desiredDifficulty chain)
  pPrint $ "nonce: " ++ (show $ _nonce (head (headers chain)))
  pPrint $ "time: " ++ (show $ timeForLastBlock chain)
  return chain

main :: IO ()
main = do
  chain <- testMining

  chain <- foldl (>>=) (return chain) (replicate 3 runStep)
  pPrint $ "balances:"
  pPrint $ balances chain
  print "done"



