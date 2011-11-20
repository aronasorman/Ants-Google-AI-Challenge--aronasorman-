module Main where

import Data.IORef

import AntLogic
import Ants
import Diffusion

doTurn :: IORef ScentedWorld -> IORef Int -> GameParams -> GameState -> IO [Order]
doTurn worldref turnRef gp gs = do
  turn <- readIORef turnRef
  plainworld <- if turn == 0 then return $ initScentedWorld (world gs) else readIORef worldref
  scentedworld <- return $ propagate $ resetWorld gs plainworld
  ownAnts <- return $ myAnts $ ants gs
  orders <- return $ map (getOrder scentedworld) ownAnts
  
  -- bookkeeping
  modifyIORef turnRef (+1) 
  writeIORef worldref plainworld
  
  return orders

main = do
  worldref <- newIORef (undefined :: ScentedWorld)
  turnNum <- newIORef 0
  game (doTurn worldref turnNum)