module Main where

import Data.List
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import System.IO

import Ants
  
type FutureOrders = Map Point Order

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrders :: World -> [Order] -> Maybe Order
tryOrders w = find (passable w)

tryOrder :: World -> Order -> Maybe Order
tryOrder world order = case passable world order of
  True -> Just order
  False -> Nothing

notInFutureOrders :: FutureOrders -> Order -> Maybe Order
notInFutureOrders future order = 
  case orderFutureLocation order `M.member` future of
    False -> Just order
    _ -> Nothing

-- | Generates orders for an Ant in all directions
-- TODO: refactor, quite a kludgy mess in here
generateOrder :: GameState -> GameParams -> FutureOrders -> Ant -> FutureOrders
generateOrder gs gp future ant = addOrder future $ pickOrder validOrders
  where
    validOrders = mapMaybe (checkIfValid . Order ant) [North,East,West,South]
    bestOrder = Order ant $ getFoodNow gp (head $ food gs) ant
    pickOrder orders = if bestOrder `elem` orders then bestOrder else head orders
    checkIfValid order = tryOrder (world gs) order >>= notInFutureOrders future 

generateOrders :: GameState -> GameParams -> [Ant] -> [Order]
generateOrders gs gp = M.elems . foldl' (generateOrder gs gp) M.empty

addOrder :: FutureOrders -> Order -> FutureOrders
addOrder future order = M.insert (orderFutureLocation order) order future

{- | 
 - Implement this function to create orders.
 - It uses the IO Monad so algorithms can call timeRemaining.
 -
 - GameParams data holds values that are constant throughout the game
 - GameState holds data that changes between each turn
 - for each see Ants module for more information
 -}
doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  -- generate orders for all ants belonging to me
  let generatedOrders = generateOrders gs gp . myAnts . ants $ gs
  -- for each ant take the first "passable" order, if one exists
  -- this shows how to check the remaining time
  elapsedTime <- timeRemaining gs
  hPutStrLn stderr $ show elapsedTime
  -- wrap list of orders back into a monad
  return generatedOrders

-- | This runs the game
main :: IO ()
main = game doTurn

-- where will my order take me?
orderFutureLocation :: Order -> Point
orderFutureLocation order = let currentPosition = point . ant $ order
                                direction' = direction order
                            in futurePosition currentPosition direction'

-- given my current position and a direction i want to go, tell me what position i'll end up
futurePosition :: Point -> Direction -> Point
futurePosition (row, col) dir =
  case dir of
    North -> (row - 1, col)
    South -> (row + 1, col)
    East -> (row, col + 1)
    West -> (row, col - 1)

-- gives the direction which will lead closest to the target
bestDirection :: GameParams -> Point -> Point -> Direction
bestDirection gp target source = minimumBy (comparing distance') [North .. West]
  where
    distance' = distance gp target . futurePosition source

getFoodNow :: GameParams -> Food -> Ant -> Direction
getFoodNow gp food ant = bestDirection gp food (point ant)