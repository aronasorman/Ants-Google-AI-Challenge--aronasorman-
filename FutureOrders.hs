module FutureOrders( 
  FutureOrders
  , addOrder
  , empty
  , finalize
  , (??)
  ) where

import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Map as M
import qualified Data.Set as S

import Ants
import Utilities

data FutureOrders = FO
                    { futureLocs :: Map Point Order
                      , currentLocs :: Set Point
                      , moving :: Set Point
                      }

addOrder :: Order -> FutureOrders -> FutureOrders
addOrder order fo = fo { futureLocs = M.insert (orderFutureLocation order) order (futureLocs fo)
                       , moving = S.insert (pointAnt $ ant order) (moving fo)
                       }

empty :: GameState -> FutureOrders
empty gs = FO M.empty (S.fromList ownants) S.empty
  where
    ownants = map pointAnt $ myAnts $ ants gs

finalize :: FutureOrders -> [Order]
finalize = map snd . M.toList . futureLocs

-- check first if the order will bump into an ant. If so, check if the ant will move.
-- then check if the future location of the ant will also bump with the future location
-- of another ant
-- | returns whether this order will bump into an ant in the future
(??) :: Order -> FutureOrders -> Bool
(??) order fo = let futureloc = orderFutureLocation order
                in if futureloc `S.member` currentLocs fo
                   then if futureloc `S.member` moving fo
                        then futureloc `M.notMember` futureLocs fo
                        else True
                   else futureloc `M.member` futureLocs fo