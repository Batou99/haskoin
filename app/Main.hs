module Main where

import Haskoin.Types
import Haskoin.Mining
import Text.Pretty.Simple
import Crypto.Hash(hashlazy)
import Data.ByteString.Lazy.Char8(pack)

getHash :: String -> HaskoinHash
getHash value = hashlazy (pack value) :: HaskoinHash

main :: IO ()
main = do
  genesis <- makeGenesis
  chain <- mineOn (return [Transaction 0 1 1000]) 2 genesis
  chain <- mineOn (return [Transaction 1 0 1000]) 2 chain

  pPrint chain
  pPrint $ mconcat $ map transactions (blocks chain)




