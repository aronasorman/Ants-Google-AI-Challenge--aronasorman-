module Main where

import Data.IORef

import AntLogic
import Ants
import Diffusion

doTurn :: IORef ScentedWorld -> IORef Int -> GameParams -> GameState -> IO [Order]
doTurn worldref turnRef gp gs = do
  turn <- readIORef turnRef
  scentedworld <- if turn == 0 then return $ initScentedWorld (world gs) else readIORef worldref
  worldwithagents <- return $ resetWorld gs scentedworld
  worldwithscent <- return $ propagate worldwithagents
  ownAnts <- return $ myAnts $ ants gs
  orders <- return $ map (getOrder worldwithscent) ownAnts
  
  -- bookkeeping
  modifyIORef turnRef (+1) 
  writeIORef worldref worldwithscent
  
  return orders

main = do
  worldref <- newIORef (undefined :: ScentedWorld)
  turnNum <- newIORef 0
  game (doTurn worldref turnNum)