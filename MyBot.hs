module Main where

import Control.Monad (liftM)
import Data.Array (assocs)
import Data.IORef (newIORef, IORef, readIORef, writeIORef)
import qualified Data.HashSet as HS
import Data.List
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import System.IO

import Ants
import Exploring
import Foraging
import Utilities  

generateOrder :: GameParams -> GameState -> AntTargets -> FutureOrders -> Ant -> FutureOrders
generateOrder gp gs ta fo ant = addOrder gs fo $ do 
  target <- M.lookup ant ta
  direction' <- movesToDirection gp gs ant target
  return $ Order ant direction'

generateOrdersAll :: GameParams -> GameState -> Unexplored -> [Ant] -> FutureOrders
generateOrdersAll gp gs unex ants = foldl' (generateOrder gp gs ta) M.empty ants
  where
    ta = assignAnts gp gs unex ants

getOrders = M.elems

assignAnts :: GameParams -> GameState -> Unexplored -> [Ant] -> AntTargets
assignAnts gp gs unex ants = let foragingAnts = forage gp gs M.empty ants
                                 unassignedAnts = unassigned foragingAnts ants
                                 exploringAnts = explore gp gs unex foragingAnts unassignedAnts
                             in if M.size foragingAnts >= M.size exploringAnts
                                then foragingAnts `M.union` exploringAnts
                                else exploringAnts `M.union` foragingAnts

unassigned :: AntTargets -> [Ant] -> [Ant]
unassigned ta = filter (\x -> M.notMember x ta)

fillUnexploredIfEmpty :: GameParams -> Unexplored -> Unexplored
fillUnexploredIfEmpty gp unexplored = if HS.null unexplored
                                      then HS.fromList $ allPoints gp
                                      else unexplored

visibleArea :: World -> Unexplored
visibleArea w = HS.fromList $ map fst $ filter (\(_,t) -> visible t) $ assocs w

doTurn :: IORef Unexplored -> GameParams -> GameState -> IO [Order]
doTurn unexploredRef gp gs = do
  unexplored <- liftM (fillUnexploredIfEmpty gp) $ readIORef unexploredRef
  generatedOrders <- return . getOrders $ generateOrdersAll gp gs unexplored $ myAnts $ ants gs
  explored <- return $ visibleArea (world gs)
  newUnexplored <- return $ unexplored `HS.difference` explored
  writeIORef unexploredRef newUnexplored
  return generatedOrders

-- | This runs the game
main :: IO ()
main = do
  unexplored <- newIORef HS.empty
  game (doTurn unexplored)