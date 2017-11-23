module Main where

import Haskoin.Types
import Haskoin.Mining
import Data.Time.Clock
import Control.Monad
import Text.Show.Prettyprint

testMining :: IO Blockchain
testMining = do
  chain <- makeGenesis
  chain <- mineOn (return []) 0 chain
  chain <- mineOn (return []) 0 chain
  chain <- mineOn (return []) 0 chain
  chain <- mineOn (return []) 0 chain
  chain <- mineOn (return []) 0 chain
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
  chain <- mineOn (return []) 0 chain
  prettyPrint $ "desired diff: " ++ (show $ desiredDifficulty chain)
  prettyPrint $ "nonce: " ++ (show $ _nonce (head (headers chain)))
  prettyPrint $ "time: " ++ (show $ timeForLastBlock chain)
  return chain

main :: IO ()
main = do
  chain <- testMining

  chain <- foldl (>>=) (return chain) (replicate 30 runStep)
  print "done"



