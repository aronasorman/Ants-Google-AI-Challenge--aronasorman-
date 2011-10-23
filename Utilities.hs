module Utilities where

{-
various functions that simply help in the ants game.
Should contain no logic whatsoever.
-}

import Control.Monad (liftM)
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

-- where will my order take me?
orderFutureLocation :: Order -> Point
orderFutureLocation order = let currentPosition = point . ant $ order
                                direction' = direction order
                            in futurePosition currentPosition direction'

-- if no other ant will go to this point, then allow this order
addOrder :: GameState -> FutureOrders -> Maybe Order -> FutureOrders
addOrder gs fo Nothing = fo -- no order was issued for this ant, so do nothing
addOrder gs fo (Just order) = 
  let p = point $ ant order
      otherAnts = ants gs
      movingAnts = map ant $ M.elems fo
      notMovingAnts = otherAnts \\ movingAnts
  in case M.lookup p fo of
    Just _ -> fo --some ant will already go to this point, so don't let this order push through
    Nothing -> case willMoveToAnAnt notMovingAnts fo order of
      Nothing -> fo
      Just o -> M.insert p o fo

-- checks if order will move ant A to the location of a given ant B. If so, check
-- if ant B will move. If Ant B will not move, then return Nothing. Else, Just order
willMoveToAnAnt :: [Ant] -> FutureOrders -> Order -> Maybe Order
willMoveToAnAnt ants fo order =
  let movingAnts = map ant $ M.elems fo
      futureLocation = orderFutureLocation order
      currentAntLocations = map (\ant' -> (ant',point ant')) ants
  in case find (\(_,p') -> futureLocation == p') currentAntLocations of
    Nothing -> return order
    Just (ant',_) -> case ant' `elem` movingAnts of 
      True -> return order
      False -> Nothing

-- given my current position and a direction i want to go, tell me what position i'll end up
futurePosition :: Point -> Direction -> Point
futurePosition (row, col) dir =
  case dir of
    North -> (row - 1, col)
    South -> (row + 1, col)
    East -> (row, col + 1)
    West -> (row, col - 1)

-- gives the direction which will lead closest to the target
-- NOTE: I presume that this will be severely modified in the future, maybe
-- to use A* search and not some anive distance algorithm.
bestDirection :: GameParams -> GameState -> Point -> Ant -> Maybe Direction
bestDirection gp gs target source = liftM direction $
                                    tryOrders (world gs) $ 
                                    map (Order source) $ 
                                    sortBy (comparing distance') [North .. West]
                                      where
                                        distance' = distance gp target . futurePosition (point source)

-- tells an ant how to go to its designated target
movesToDirection :: GameParams -> GameState -> Ant -> Point -> Maybe Direction
movesToDirection gp gs ant point' = bestDirection gp gs point' ant