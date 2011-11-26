{-|
Records the last time each cell was last seen
|-}
module Unexplored ( Unexplored
                  , LastSeen
                  , initUnexplored
                  , updateUnexplored
                  , lastSeen
                  ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Int

import Ants

type LastSeen = Int8

type Unexplored = UArray Point LastSeen

type MUnexplored s = STUArray s Point LastSeen

initUnexplored :: World -> Unexplored
initUnexplored w = runSTUArray $ do 
  mworld <- newArray (bounds w) 0
  return mworld
  
updateUnexplored :: World -> Unexplored -> Unexplored
updateUnexplored w unex = runSTUArray $ do
  mworld <- thaw unex
  forM_ (assocs w) $ \(p,t) -> do
    if visible t
      then reset p mworld
      else increment p mworld
  return mworld

-- | returns the number of turns a cell was last seen
-- | Note: Doesn't check if the point is outside the map!
lastSeen :: Point -> Unexplored -> LastSeen
lastSeen p w = w ! p

reset :: Point -> MUnexplored s -> ST s ()
reset p w = writeArray w p 0

increment :: Point -> MUnexplored s  -> ST s ()
increment p w = do
  oldval <- readArray w p
  writeArray w p (oldval + 1)