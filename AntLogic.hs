module AntLogic where

import Data.List
import Data.Ord (comparing)

import Ants hiding (Water)
import DiffusionM
import FutureOrders

evaluate world current evaled = if scent EnemyHill current > 0
                                   then scent EnemyHill evaled 
                                else if scent EnemyAnt current > 0 
                                     then if scent OwnAnt current > 4
                                          then scent EnemyAnt evaled
                                          else scent OwnAnt evaled
                                     else if scent Food current > 0 
                                          then scent Food evaled
                                          else scent Unexplored evaled
           
-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrders :: FutureOrders -> World -> [Order] -> Maybe Order
tryOrders fo w = find valid
  where
    valid order = not (order ?? fo) && passable w order

getOrder :: World -> ScentedWorld -> FutureOrders -> Ant -> FutureOrders
getOrder oworld world fo ant = let directions = [North .. West]
                                   currentPoint = pointAnt ant
                                   currentTile = getTile currentPoint world
                                   eval = {-# SCC "eval" #-} evaluate world currentTile 
                                                             . flip getTile world 
                                                             . flip move currentPoint
                                   order = tryOrders fo oworld
                                           $ map (Order ant) 
                                           $ reverse 
                                           $ sortBy (comparing eval) directions
                               in case order of
                                 Nothing -> fo
                                 Just o -> addOrder o fo