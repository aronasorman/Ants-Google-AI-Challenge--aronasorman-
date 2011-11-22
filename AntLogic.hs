module AntLogic where

import Data.List
import Data.Ord (comparing)

import Ants
import Diffusion
import FutureOrders
import Utilities

evaluate :: ScentedTile -> Double
evaluate tile = scent Food tile + scent EnemyHill tile + 0.4 * scent EnemyAnt tile

getOrder :: ScentedWorld -> FutureOrders -> Ant -> FutureOrders
getOrder world fo ant = let directions = [North .. West]
                            currentPoint = pointAnt ant
                            eval = evaluate . (\x -> getTile x world) . (\x -> move x $ pointAnt ant)
                            sortedorders = map (Order ant) $ reverse $ sortBy (comparing eval) directions
                        in case find (not . (??fo)) sortedorders of
                          Nothing -> fo
                          Just order -> addOrder order fo