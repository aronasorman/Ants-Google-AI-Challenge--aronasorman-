{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
module Diffusion(
  -- types
  Agent(..)
  , Scent
  , ScentedTile
  , ScentedWorld
    
    -- constants
  , diff_rate
  , diff_max
  , diff_min
    
    -- functions
  , initScentedWorld
  , resetWorld
  , propagate
  , scent
  , getTile
  ) where

import Control.Monad
import Data.Array
import Data.List
import Data.Map (Map)

import qualified Data.Map as M
  
import Ants

-- CONSTANTS

diff_rate = 0.25

diff_max = 200

diff_min = 0.001 :: ScentStrength

propagate_length = 9

-- TYPES

-- | Agents are items that release their own scent
data Agent = Food | Own | Enemy | OwnHill | EnemyHill
           deriving (Eq, Ord, Show, Enum, Bounded)

newtype Scent = Scent Agent
           deriving (Ord, Show)

data TileType = LandTile | WaterTile
              deriving (Eq, Ord, Show)

instance Eq Scent where
  (Scent a) == (Scent b) = a == b

data ScentedTile = ScentedTile {
  agents :: Maybe AgentStack
  , landtype :: TileType
  , scents :: ScentStack
  } deriving (Eq, Ord, Show)

type AgentStack = Agent

type ScentStack = Map Scent ScentStrength

type ScentedWorld = Array Point ScentedTile

type Lambda = Double

type ScentStrength = Double

-- exposed functions
                     
-- | initialize the world, setting the land type of each tile
initScentedWorld :: World -> ScentedWorld
initScentedWorld w = array (bounds w) [(p,initTile $ tile t) | (p,t) <- assocs w]


-- | remove the agents from a previous turn, and place the location of each agent
resetWorld :: GameState -> ScentedWorld -> ScentedWorld
resetWorld gs = placeAgents gs . clearAgents
                
-- | propagate the scent of each agent
propagate :: ScentedWorld -> ScentedWorld
propagate =  propagate' propagate_length
             
-- | returns the scent of a specific agent on a tile, but returns 0
-- if the current scent is less than diff_min
scent :: Agent -> ScentedTile -> ScentStrength
scent agent tile = let val = M.findWithDefault 0 (Scent agent) $ scents tile
                   in if val <= diff_min
                      then 0
                      else val

getTile :: Point -> ScentedWorld -> ScentedTile
getTile = flip (%!)

                    
---
--- Implementation
---
    
addAgent :: Agent -> ScentedTile -> ScentedTile
addAgent agent tile = tile { agents = Just agent }

addScent :: Agent -> Double -> ScentedTile -> ScentedTile
addScent agent strength tile = let scent = scents tile
                               in tile { scents = M.insert (Scent agent) strength scent }

clearScent :: ScentedTile -> ScentedTile
clearScent tile = tile { scents = M.empty }

clearAgent :: ScentedTile -> ScentedTile
clearAgent tile = tile { agents = Nothing }

clearAgents :: ScentedWorld -> ScentedWorld
clearAgents w = w // [(p, clearAgent t) | (p,t) <- assocs w]

-- | Initialize a scented tile's landtype based on a Tile. (initialize if water or land)
initTile :: Tile -> ScentedTile
initTile tile =
  case tile of
    Water -> ScentedTile Nothing WaterTile M.empty
    _ -> ScentedTile Nothing LandTile M.empty

-- | Place own ants and food, plus their scents,  on the scented world
-- NOTE: EDIT IF YOU WANT TO TRACK MORE AGENTS
placeAgents :: GameState -> ScentedWorld -> ScentedWorld
placeAgents gs = placeOwnAnts . placeFood
  where
    placeFood = placeItem (food gs) Food
    placeOwnAnts = placeItem (map point $ myAnts $ ants gs) Own

placeItem :: [Point] -> Agent -> ScentedWorld -> ScentedWorld
placeItem points agent w =
  w // [(p,t') | p <- points, t' <- return $ addScent agent diff_max $ addAgent agent $ w%!p]

colBound :: ScentedWorld -> Col
colBound = col . snd . bounds

rowBound :: ScentedWorld -> Row
rowBound = row . snd . bounds

-- Takes the modulus of the indices before accessing the array
(%!) :: ScentedWorld -> Point -> ScentedTile
(%!) w p = w ! (w %!% p)

(%!%) :: ScentedWorld -> Point -> Point
(%!%) w p = 
  let modCol = {-# SCC "modCol" #-} 1 + colBound w
      modRow = {-# SCC "modRow" #-} 1 + rowBound w
      ixCol  = {-# SCC "ixCol" #-} col p `mod` modCol
      ixRow  = {-# SCC "ixRow" #-} row p `mod` modRow
  in (ixRow, ixCol)

isWater :: Point -> ScentedWorld -> Bool
isWater p = (==WaterTile) . landtype . (%!p)

-- | Returns all neighboring points that aren't water
neighboringPoints :: ScentedWorld -> Point -> [Point]
neighboringPoints w p = filter (not . flip isWater w) . map (flip move p) $ [North .. West]

-- | The diffusion equation used to compute the diffusion value of each tile
diffusion :: Point -> Agent -> ScentedWorld -> Double
diffusion point agent world = 
  let neighborTiles = map (world%!) $ neighboringPoints world point
      thisTile = world %! point
      diffusionvals = map (\x -> scent agent x - scent agent thisTile) neighborTiles
      summation = sum diffusionvals
  in scent agent thisTile + diff_rate * summation

propagate1 :: ScentedWorld -> ScentedWorld
propagate1 w = (w//) $ do
  (p,t) <- assocs w
  agent <- [Food .. Own]
  let diffval = diffusion p agent w
      currentval = scent agent t
      t' = addScent agent diffval t
   in do
    guard $ diffval > currentval
    return (p,t')
    
propagate' :: Int -> ScentedWorld -> ScentedWorld
propagate' 0 = id
propagate' n = propagate' (n - 1) . propagate1