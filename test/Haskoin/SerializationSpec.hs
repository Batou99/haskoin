module Haskoin.SerializationSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Haskoin.Serialization
import Haskoin.Types
import qualified Data.ByteString.Base16.Lazy as BSL

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Genesis" $ do
    it "Serializes Genesis" $ do
      let genesis = Genesis (Block [])
      BSL.encode(serialize genesis) `shouldBe` "000000000000000000"
