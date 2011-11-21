module FutureOrders( 
  FutureOrders
  , addOrder
  , empty
  , finalize
  , (??)
  ) where

import Data.Set (Set)

import qualified Data.Set as S

import Ants
import Utilities

type FutureOrders = Set Order

instance Eq Order where
  a == b = orderFutureLocation a == orderFutureLocation b

instance Ord Order where
  compare a b = orderFutureLocation a `compare` orderFutureLocation b

addOrder :: Order -> FutureOrders -> FutureOrders
addOrder order = S.insert order

empty :: FutureOrders
empty = S.empty

finalize :: FutureOrders -> [Order]
finalize = S.toList

(??) :: Order -> FutureOrders -> Bool
(??) = S.member