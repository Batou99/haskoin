module Main where

import Haskoin.Types
import Text.Pretty.Simple
import Crypto.Hash(hashlazy)
import Data.ByteString.Lazy.Char8(pack)

getHash :: String -> HaskoinHash
getHash value = hashlazy (pack value) :: HaskoinHash

main :: IO ()
main = do
  let genesisBlock = Genesis (Block [])
      firstBlock   = Node (Block [Transaction 0 1 1000]) 
                        BlockHeader { _minerAccount = 0, _parentHash = getHash "genesisHash" } 
                        genesisBlock
      chain        = Node (Block [Transaction 1 0 1000]) 
                        BlockHeader { _minerAccount = 0, _parentHash = getHash "firstHash" } 
                        firstBlock
      txns         = mconcat $ map transactions (blocks chain)

  pPrint chain
  pPrint txns




