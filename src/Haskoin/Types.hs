{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Haskoin.Types where

import Protolude
import Crypto.Hash

type Account = Integer

data Transaction = Transaction {
    _from :: Account,
    _to :: Account,
    _amount :: Integer
  } deriving (Eq, Show)

newtype Block = Block [Transaction] deriving (Eq, Show, Monoid)

type HaskoinHash = Digest SHA1 

data BlockHeader = BlockHeader {
    _miner :: Account,
    _parentHash :: HaskoinHash
  } deriving (Eq, Show)


data Blockchain = Genesis Block
                | Node Block BlockHeader Blockchain
                deriving (Eq)

deriving instance Show Blockchain
