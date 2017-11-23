{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Haskoin.Types where

import Crypto.Hash
import Data.Time.Clock.POSIX

type Account = Integer

data Transaction = Transaction {
    _from :: Account,
    _to :: Account,
    _amount :: Integer
  } deriving (Eq, Show)

newtype Block = Block [Transaction] deriving (Eq, Show, Monoid)

type HaskoinHash = Digest SHA1 

data BlockHeader = BlockHeader {
    _minerAccount :: Account,
    _parentHash :: HaskoinHash,
    _nonce :: Integer,
    _minedAt :: POSIXTime
  } deriving (Eq, Show)


data Blockchain = Genesis Block
                | Node Block BlockHeader Blockchain
                deriving (Eq, Show)


blocks :: Blockchain -> [Block]
blocks (Genesis block) = [block]
blocks (Node block _ parent) = block : blocks parent


headers :: Blockchain -> [BlockHeader]
headers (Genesis _) = []
headers (Node _ header parent) = header : headers parent


transactions :: Block -> [Transaction]
transactions (Block ts) = ts
