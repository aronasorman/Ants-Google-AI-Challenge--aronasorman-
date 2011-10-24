module Exploring(explore) where

import Control.Monad (mplus)
import Data.HashSet (toList)
import Data.List
import Data.Ord

import qualified Data.Map as M

import Ants
import Utilities

increment = 3

type UnexploredList = [Point]

-- find the nearest unexplored area for each ant
explore :: GameParams -> GameState -> Unexplored -> AntTargets -> [Ant] -> AntTargets
explore gp gs unex ta ants = let unexList = toList unex
                             in case M.null ta of
                               _ -> foldl' (exploreMap gp unexList) ta ants

exploreMap :: GameParams -> UnexploredList -> AntTargets -> Ant -> AntTargets 
exploreMap gp unexList ta ant' = let targetPoint = nearestUnexplored gp unexList ant'
                                 in case targetPoint of
                                   Nothing -> ta
                                   Just p -> M.insert ant' p ta

nearestUnexplored :: GameParams -> UnexploredList -> Ant -> Maybe Point
nearestUnexplored gp [] _ = Nothing
nearestUnexplored gp unexList ant' = return $ minimumBy (comparing distance') unexList
  where
    distance' p = distance gp p (point ant')