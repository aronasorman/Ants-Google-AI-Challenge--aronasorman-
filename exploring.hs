module Exploring(explore) where

import Control.Monad (mplus)
import Data.List
import Data.Ord

import qualified Data.Map as M

import Ants
import Utilities

-- for now, follow the previously allocated ant who is the closest. For now.
-- if there was no one allocated, then order the ants to go 10 rows north. If we hit
-- an obstacle, then go south. Haha.
explore :: GameParams -> GameState -> AntTargets -> [Ant] -> AntTargets
explore gp gs ta ants = case M.null ta of
  _ -> foldl' (exploreMap gs) ta ants

-- ant simply bobs up or down, as long as the target is not water
exploreMap :: GameState -> AntTargets -> Ant -> AntTargets 
exploreMap gs ta ant' = case goUpOrDown gs ant' `mplus` goLeftOrRight gs ant' of
  Nothing -> ta
  Just p -> M.insert ant' p ta

goUpOrDown :: GameState -> Ant -> Maybe Point
goUpOrDown gs ant' = 
  find (not . isWater w) $ map (toPoint . incAntRow) increments
    where
      w = world gs
      toPoint row' = (row', col . point $ ant')
      increments = [-10,10]
      incAntRow inc = (+inc) . row . point $ ant'

goLeftOrRight :: GameState -> Ant -> Maybe Point
goLeftOrRight gs ant' = find (not . isWater w) $ map (toPoint . incAntCol) increments
    where
      w = world gs
      toPoint col' = (row . point $ ant', col')
      increments = [-10,-5,5,10]
      incAntCol inc = (+inc) . col . point $ ant'
      
closestAnt :: GameParams -> [Ant] -> Point -> Maybe Ant
closestAnt _ [] _ = Nothing
closestAnt gp ants food' = return . head $ sortBy (comparing distance') ants
  where
    distance' ant = distance gp (point ant) food'
