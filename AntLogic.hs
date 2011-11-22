module AntLogic where

import Data.List
import Data.Ord (comparing)

import Ants
import Diffusion
import Utilities

evaluate :: ScentedTile -> Double
evaluate tile = scent Food tile

getOrder :: ScentedWorld -> Ant -> Order
getOrder world ant = let directions = [North .. West]
                         currentPoint = pointAnt ant
                         eval = evaluate . (\x -> getTile x world) . (\x -> move x $ pointAnt ant)
                     in Order ant $ maximumBy (comparing eval) directions