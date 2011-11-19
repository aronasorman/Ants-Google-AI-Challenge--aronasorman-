module AntLogic where

import Data.List
import Data.Ord (comparing)

import Ants
import Diffusion
import Utilities

evaluate :: ScentedTile -> Double
evaluate = scentStrength Food

getOrder :: Ant -> ScentedWorld -> Order
getOrder ant world = let directions = [North .. West]
                         currentPoint = point ant
                         eval = evaluate . flip getTile world . futurePosition currentPoint 
                     in Order ant $ maximumBy (comparing eval) directions