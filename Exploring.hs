module Exploring(explore) where

import Control.Monad (mplus)
import Data.List
import Data.Ord

import qualified Data.Map as M

import Ants
import Utilities

increment = 3

-- for now, follow the previously allocated ant who is the closest. For now.
-- if there was no one allocated, then order the ants to go 10 rows north. If we hit
-- an obstacle, then go south. Haha.
explore :: GameParams -> GameState -> AntTargets -> [Ant] -> AntTargets
explore gp gs ta ants = case M.null ta of
  _ -> foldl' (exploreMap gs) ta ants

-- ant simply bobs up or down, as long as the target is not water
exploreMap :: GameState -> AntTargets -> Ant -> AntTargets 
exploreMap gs ta ant' = M.insert ant' dir ta
  where
    dir = directionWithLeastObstacles gs ant'

directionWithLeastObstacles :: GameState -> Ant -> Point
directionWithLeastObstacles gs ant' = 
  head $ sortBy (comparing $ countObstacles gs ant') $ reverse allPoints
    where
      allPoints = [increment,-increment] >>= makePoint
      makePoint a = map ((,) a) [increment, -increment]

countObstacles :: GameState -> Ant -> Point -> Int
countObstacles gs ant' p = 
  let pAnt = point ant'
  in if pAnt == p
     then 0
     else let rowDiff = case (row pAnt - row p) `compare` 0 of
                GT -> 1
                LT -> -1
                EQ -> 0
              colDiff = case (col pAnt - col p) `compare` 0 of
                GT -> 1
                LT -> -1
                EQ -> 0
              inc = if isWater (world gs) p then 1 else 0
          in inc + countObstacles gs ant' (row p + rowDiff, col p + colDiff)
                  
closestAnt :: GameParams -> [Ant] -> Point -> Maybe Ant
closestAnt _ [] _ = Nothing
closestAnt gp ants food' = return . head $ sortBy (comparing distance') ants
  where
    distance' ant = distance gp (point ant) food'
