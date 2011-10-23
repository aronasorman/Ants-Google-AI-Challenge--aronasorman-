module Main where

import Control.Monad (liftM)
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
generateOrder gp gs ta fo ant = addOrder fo $ do 
  target <- M.lookup ant ta
  direction' <- movesToDirection gp gs ant target
  return $ Order ant direction'

generateOrdersAll :: GameParams -> GameState -> [Ant] -> FutureOrders
generateOrdersAll gp gs ants = foldl' (generateOrder gp gs ta) M.empty ants
  where
    ta = assignAnts gp gs ants

getOrders = M.elems

assignAnts :: GameParams -> GameState -> [Ant] -> AntTargets
assignAnts gp gs ants = let foragingAnts = forage gp gs M.empty ants
                            unassignedAnts = unassigned foragingAnts ants
                            exploringAnts = explore gp gs foragingAnts unassignedAnts
                        in if M.size foragingAnts >= M.size exploringAnts
                           then foragingAnts `M.union` exploringAnts
                           else exploringAnts `M.union` foragingAnts

unassigned :: AntTargets -> [Ant] -> [Ant]
unassigned ta = filter (\x -> M.notMember x ta)

doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  -- generate orders for all ants belonging to me
  let generatedOrders = getOrders $ generateOrdersAll gp gs $ myAnts $ ants gs
  -- for each ant take the first "passable" order, if one exists
  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return generatedOrders

-- | This runs the game
main :: IO ()
main = game doTurn