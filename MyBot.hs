module Main where

import Data.IORef
import Data.List

import AntLogic
import Ants
import DiffusionM
import FutureOrders
import Unexplored

doTurn :: IORef Int -> IORef Unexplored -> GameParams -> GameState -> IO [Order]
doTurn turnRef unexRef gp gs = do
  turn <- readIORef turnRef
  unex <- if turn == 0 then return $ initUnexplored (world gs) else readIORef unexRef
  plainworld <- return $ initScentedWorld (world gs)
  scentedworld <- return $ propagate (world gs) $ resetWorld gs unex plainworld
  ownAnts <- return $ myAnts $ ants gs
  orders <- return $ finalize $ foldl' (getOrder (world gs) scentedworld) (empty gs) ownAnts
  
  -- bookkeeping
  modifyIORef unexRef (updateUnexplored (world gs))
  modifyIORef turnRef (+1) 
  
  return orders

main = do
  turnRef <- newIORef 0
  unexRef <- newIORef undefined
  game (doTurn turnRef unexRef)