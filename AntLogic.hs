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
                                      in 1.5 * scent Food tile 
                                         + 1 * scent EnemyHill tile 
                                         + 1.5 * scent OwnAnt tile 
                                         + 0.4 * scent EnemyAnt tile

scatterAndExplore :: ScentedWorld -> Point -> Double
scatterAndExplore world point = let tile = getTile point world
                               in 1 * scent Food tile 
                                  + 1 * scent EnemyHill tile
                                  - 0.2 * scent OwnAnt tile

attackTogether :: ScentedWorld -> Point -> Double
attackTogether world point = let tile = getTile point world
                             in 3.5 * scent OwnAnt tile
                                + 1 * scent EnemyHill tile
                                + 1 * scent EnemyAnt tile

evaluate :: ScentedWorld -> Point -> Point -> Double
evaluate world currentpoint evaledpoint = 
  let currenttile = getTile currentpoint world
      evaledtile = getTile evaledpoint world
  in if nTilesAway 4 EnemyAnt world currentpoint > 0
     then attackTogether world evaledpoint
     else if nTilesAway 1 OwnAnt world currentpoint > 0
          then scatterAndExplore world evaledpoint
          else gatherFoodStickTogether world evaledpoint

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