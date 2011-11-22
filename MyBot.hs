module Main where

import Data.IORef
import Data.List

import AntLogic
import Ants
import Diffusion
import FutureOrders

doTurn :: IORef ScentedWorld -> IORef Int -> GameParams -> GameState -> IO [Order]
doTurn worldref turnRef gp gs = do
  turn <- readIORef turnRef
  plainworld <- if turn == 0 then return $ initScentedWorld (world gs) else readIORef worldref
  scentedworld <- return $ propagate $ resetWorld gs plainworld
  ownAnts <- return $ myAnts $ ants gs
  orders <- return $ finalize $ foldl' (getOrder scentedworld) empty ownAnts
  
  -- bookkeeping
  modifyIORef turnRef (+1) 
  writeIORef worldref plainworld
  
  return orders

main = do
  worldref <- newIORef (undefined :: ScentedWorld)
  turnNum <- newIORef 0
  game (doTurn worldref turnNum)