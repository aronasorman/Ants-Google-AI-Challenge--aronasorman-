module AntLogic where

import Data.List
import Data.Ord (comparing)

import Ants
import Diffusion
import FutureOrders
import Utilities

nTilesAway :: Int -> Agent -> ScentedWorld -> Point -> Double
nTilesAway 0 agent world point = let tile = getTile point world
                                 in if scent agent tile >= 0.5 * diff_max
                                    then 1
                                    else 0
nTilesAway n agent world point = 
  let neighbors = neighboringPoints world point
      nearness p = scent agent $ getTile p world
  in nTilesAway (n-1) agent world $ maximumBy (comparing nearness) neighbors

gatherFoodStickTogether :: ScentedWorld -> Point -> Double
gatherFoodStickTogether world point = let tile = getTile point world
                                      in 8.6 * scent Food tile 
                                         + 20 * scent EnemyHill tile 
                                         + 0.1 * scent OwnAnt tile 
                                         + 0.4 * scent EnemyAnt tile

gatherAndExplore :: ScentedWorld -> Point -> Double
gatherAndExplore world point = let tile = getTile point world
                               in 1 * scent Food tile 
                                  + 2 * scent EnemyHill tile 
                                  + 0.1 * nTilesAway 4 OwnAnt world point
                                  - 3.5 * nTilesAway 2 OwnAnt world point
                                  + 0.4 * scent EnemyAnt tile

evaluate :: ScentedWorld -> Point -> Point -> Double
evaluate world currentpoint evaledpoint = let currenttile = getTile currentpoint world
                                              evaledtile = getTile evaledpoint world
                                          in if nTilesAway 3 OwnAnt world currentpoint > 0
                                             then gatherFoodStickTogether world evaledpoint
                                             else gatherAndExplore world evaledpoint

passable' :: ScentedWorld -> Order -> Bool
passable' world order = let newPoint = move (direction order) (pointAnt $ ant order)
                        in agents (getTile newPoint world) /= Just Diffusion.Water

getOrder :: ScentedWorld -> FutureOrders -> Ant -> FutureOrders
getOrder world fo ant = let directions = [North .. West]
                            currentPoint = pointAnt ant
                            currentTile = getTile currentPoint world
                            eval = evaluate world currentPoint . flip move currentPoint
                            sortedorders = filter (passable' world)
                                           $ map (Order ant) 
                                           $ reverse 
                                           $ sortBy (comparing eval) directions
                        in case find (not . (??fo)) sortedorders of
                          Nothing -> fo
                          Just order -> addOrder order fo