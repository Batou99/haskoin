{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}

module Haskoin.Serialization where

import GHC.Generics
import Data.Binary as B
import Data.Binary.Get
import Data.ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Haskoin.Types
import Control.Monad
import Crypto.Hash
import Data.Time.Clock.POSIX

deriving instance Generic BlockHeader
deriving instance Generic Transaction
deriving instance Generic Blockchain
deriving instance Generic Block

instance Binary BlockHeader
instance Binary Transaction
instance Binary Blockchain
instance Binary Block

instance Binary HaskoinHash where
  get = do
    mDigest <- digestFromByteString <$> (B.get :: Get BS.ByteString)
    case mDigest of
      Nothing -> fail "Not a valid digest"
      Just digest -> return digest
  put digest = B.put $ (convert digest :: BS.ByteString)

instance Binary POSIXTime where
  get = fromInteger <$> (B.get :: Get Integer)
  put x = B.put $ (round x :: Integer)


deserialize :: BSL.ByteString -> Blockchain
deserialize = B.decode

serialize :: Blockchain -> BSL.ByteString
serialize = B.encode
