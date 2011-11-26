{-|
Records the last time each cell was last seen
|-}
module Unexplored where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Int

import Ants

type Unexplored = UArray Point Int8

type MUnexplored s = STUArray s Point Int8

initUnexplored :: GameState -> World -> Unexplored
initUnexplored gs w = runSTUArray $ do 
  mworld <- newArray (bounds w) 0
  return mworld

reset :: Point -> MUnexplored s -> ST s ()
reset p w = writeArray w p 0

increment :: Point -> MUnexplored s  -> ST s ()
increment p w = do
  oldval <- readArray w p
  writeArray w p (oldval + 1)

updateUnexplored :: World -> Unexplored -> Unexplored
updateUnexplored w unex = runSTUArray $ do
  mworld <- unsafeThaw unex
  forM_ (assocs w) $ \(p,t) -> do
    if visible t
      then reset p mworld
      else increment p mworld
  return mworld