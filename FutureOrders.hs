module FutureOrders( 
  FutureOrders
  , addOrder
  , empty
  , finalize
  , (??)
  ) where

import Data.Map (Map)

import qualified Data.Map as M

import Ants
import Utilities

type FutureOrders = Map Point Order

addOrder :: Order -> FutureOrders -> FutureOrders
addOrder order fo = M.insert loc order fo
  where
    loc = orderFutureLocation order

empty :: FutureOrders
empty = M.empty

finalize :: FutureOrders -> [Order]
finalize = map snd . M.toList

(??) :: Order -> FutureOrders -> Bool
(??) order = M.member $ orderFutureLocation order