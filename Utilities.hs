module Utilities where

{-
various functions that simply help in the ants game.
Should contain no logic whatsoever.
-}

import Control.Monad (liftM)
import Data.IORef (IORef)
import Data.Map (Map)
import Data.List
import Data.Ord (comparing)
import qualified Data.Map as M

import Ants

type FutureOrders = Map Point Order -- avoid two ants from stepping on each other

type AntTargets = Map Ant Point -- datastructure to represent an ant and its designated target

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrders :: World -> [Order] -> Maybe Order
tryOrders w = find (passable w)

orderFutureLocation :: Order -> Point
orderFutureLocation order = let currentPosition = pointAnt . ant $ order
                                direction' = direction order
                            in futurePosition currentPosition direction'

-- given my current position and a direction i want to go, tell me what position i'll end up
futurePosition :: Point -> Direction -> Point
futurePosition (row, col) dir =
  case dir of
    North -> (row - 1, col)
    South -> (row + 1, col)
    East -> (row, col + 1)
    West -> (row, col - 1)