module AntLogic where

import Data.List
import Data.Ord (comparing)

import Ants hiding (Water)
import DiffusionM
import FutureOrders

nTilesAway :: Int -> Agent -> ScentedTile -> Double
nTilesAway x agent tile = let count 0 = id
                              count n = count (n - 1) . (*diff_rate)
                          in if count x diff_max <= 0.5 * scent agent tile
                             then 1
                             else 0

gatherFoodStickTogether :: ScentedWorld -> ScentedTile -> Double
gatherFoodStickTogether world tile = 1.5 * scent Food tile 
                                     + 1 * scent EnemyHill tile 
                                     + 1.5 * scent OwnAnt tile 
                                     + 0.4 * scent EnemyAnt tile

scatterAndExplore :: ScentedWorld -> ScentedTile -> Double
scatterAndExplore world tile = 1 * scent Food tile 
                               + 1 * scent EnemyHill tile
                               - 0.01 * scent OwnAnt tile

attackTogether :: ScentedWorld -> ScentedTile -> Double
attackTogether world tile = 3.5 * scent OwnAnt tile
                            + 1 * scent EnemyHill tile
                            + 1 * scent EnemyAnt tile
                            + 1 * scent OwnAnt tile
                            
evaluate world current evaled = scatterAndExplore world evaled
           
passable' :: ScentedWorld -> Order -> Bool
passable' world order = let newPoint = move (direction order) (pointAnt $ ant order)
                        in agents (getTile newPoint world) /= Just DiffusionM.Water
                           
-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrders :: ScentedWorld -> [Order] -> Maybe Order
tryOrders w = find (passable' w)

getOrder :: ScentedWorld -> FutureOrders -> Ant -> FutureOrders
getOrder world fo ant = let directions = [North .. West]
                            currentPoint = pointAnt ant
                            currentTile = getTile currentPoint world
                            eval = {-# SCC "eval" #-} evaluate world currentTile 
                                                      . flip getTile world 
                                                      . flip move currentPoint
                            order = tryOrders world
                                    $ map (Order ant) 
                                    $ reverse 
                                    $ sortBy (comparing eval) directions
                        in case order of
                          Nothing -> fo
                          Just thereisorder -> case thereisorder ?? fo of
                            True -> fo
                            False -> addOrder thereisorder fo
                            