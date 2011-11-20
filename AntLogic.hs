module AntLogic where

import Data.List
import Data.Ord (comparing)

import Ants
import Diffusion
import Utilities

evaluate :: ScentedTile -> Double
evaluate tile = scent Food tile - 0.5 * scent Own tile

getOrder :: ScentedWorld -> Ant -> Order
getOrder world ant = let directions = [North .. West]
                         currentPoint = point ant
                         eval = evaluate . flip getTile world . futurePosition currentPoint 
                     in Order ant $ maximumBy (comparing eval) directions