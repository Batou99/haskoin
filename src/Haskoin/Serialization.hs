{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Haskoin.Serialization where

import Data.Binary as B
import Data.Binary.Get
import Data.ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Haskoin.Types
import Protolude
import Control.Monad
import Crypto.Hash

deriving instance Generic BlockHeader
deriving instance Generic Transaction
deriving instance Generic Blockchain
deriving instance Generic Block

instance Binary BlockHeader where
instance Binary Transaction where
instance Binary Blockchain
instance Binary Block

instance Binary HaskoinHash where
  get = do
    mDigest <- digestFromByteString <$> (B.get :: Get BS.ByteString)
    case mDigest of
      Nothing -> fail "Not a valid digest"
      Just digest -> return digest
  put digest = B.put $ (convert digest :: BS.ByteString)

deserialize :: BSL.ByteString -> Blockchain
deserialize = B.decode

serialize :: Blockchain -> BSL.ByteString
serialize = B.encode

