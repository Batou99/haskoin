module Haskoin.MiningSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Haskoin.Types
import Haskoin.Mining
import Haskoin.Serialization
import Protolude
import Crypto.Hash

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "makeGenesis" $ do
    it "Generates a Genesis block" $ do
      genesis <- makeGenesis
      genesis `shouldBe` Genesis (Block [])

  describe "mineOn" $ do
    it "Generates a chain" $ do
      let txnPool                         = return []
          genesisBlock                    = Genesis (Block [])
          extractParent (Node _ _ parent) = parent
          extractHeader (Node _ header _) = header

      genesis <- makeGenesis
      first   <- mineOn txnPool 0 genesis
      second  <- mineOn txnPool 0 first

      genesis `shouldBe` genesisBlock
      extractParent first `shouldBe` genesis
      extractParent second `shouldBe` first

      _parentHash (extractHeader first) `shouldBe` hashlazy(serialize genesis)
      _parentHash (extractHeader second) `shouldBe` hashlazy(serialize first)


