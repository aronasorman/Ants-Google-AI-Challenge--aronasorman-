module Foraging(forage) where

{- 
Module tasked with finding suitable food and
telling the ant to go get it. I separated it into
its own module to unclutter the MyBot file.
-}

import Data.List
import Data.Maybe (mapMaybe,isJust)
import Data.Ord
import qualified Data.Map as M

import Ants
import Utilities

forage :: GameParams -> GameState -> AntTargets -> [Ant] -> AntTargets
forage gp gs ta ants = M.fromList $ mapMaybe matchToAnt (food gs)
  where
    matchToAnt food' = let ant' = closestAnt gp ants food'
                       in case ant' of
                         Just a -> return (a,food')
                         _ -> Nothing

closestAnt :: GameParams -> [Ant] -> Food -> Maybe Ant
closestAnt _ [] _ = Nothing
closestAnt gp ants food' = return . head $ sortBy (comparing distance') ants
  where
    distance' ant = distance gp (point ant) food'