module Unexplored where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Int

import Ants

type Unexplored = UArray Point Int8

type MUnexplored s = STUArray s Point Int8

highestUnexplored :: 

initUnexplored :: GameState -> World -> Unexplored
initUnexplored gs w = runSTUArray $ do 
  mworld <- newArray (bounds w) 1
  prioritizeHills gs mworld
  return mworld

prioritize :: Point -> MUnexplored s -> ST s ()
prioritize point mworld = do 
  oldVal <- readArray mworld point
  writeArray mworld point $ oldVal + 5

prioritizeHills :: GameParams -> MUnexplored s -> ST s ()
prioritizeHills gs w = do 
  forM_ (hills gs) $ \hill -> do
    point <- return $ pointHill hill
    undefined

advance :: MUnexplored s -> ST s ()
advance w = do 
  bounds <- getAssocs w
  forM_ bounds $ \(p,i) -> do
  writeArray w p (i + 1)