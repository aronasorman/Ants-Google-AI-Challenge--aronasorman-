module Main where

import Data.IORef
import Data.List

import AntLogic
import Ants
import DiffusionM
import FutureOrders

doTurn :: IORef ScentedWorld -> IORef Int -> GameParams -> GameState -> IO [Order]
doTurn worldref turnRef gp gs = do
  turn <- readIORef turnRef
  world <- return $ initScentedWorld (world gs)
  scentedworld <- return $ propagate $ resetWorld gs world
  ownAnts <- return $ myAnts $ ants gs
  orders <- return $ finalize $ foldl' (getOrder scentedworld) empty ownAnts
  
  -- bookkeeping
  modifyIORef turnRef (+1) 
  
  return orders

main = do
  worldref <- newIORef (undefined :: ScentedWorld)
  turnNum <- newIORef 0
  game (doTurn worldref turnNum)