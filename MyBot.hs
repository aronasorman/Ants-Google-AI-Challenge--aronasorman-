module Main where


import AntLogic
import Ants
import Diffusion

doTurn :: GameParams -> GameState -> IO [Order]
doTurn gp gs = do
  world <- return $ processWorld gs
  ownAnts <- return $ myAnts $ ants gs
  return $ map (getOrder world) ownAnts

main = game doTurn