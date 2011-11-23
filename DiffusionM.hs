module Diffusion where

import Data.Array
import Data.Array.ST
import Data.Map (Map)

import qualified Data.Map as M

import Ants

-- TYPES

-- | Agents are items that release their own scent
data Agent = Water | Food | OwnAnt | EnemyAnt | OwnHill | EnemyHill
           deriving (Eq, Ord, Show, Enum, Bounded)

newtype Scent = Scent Agent
           deriving (Ord, Show)

data TileType = LandTile | WaterTile
              deriving (Eq, Ord, Show)

instance Eq Scent where
  (Scent a) == (Scent b) = a == b

data ScentedTile = ScentedTile {
  agents :: Maybe AgentStack
  , scents :: ScentStack
  } deriving (Eq, Ord, Show)

type AgentStack = Agent

type ScentStack = Map Scent ScentStrength

type ScentedWorld = Array Point ScentedTile

type MWorld s = STArray s Point ScentedTile

type ScentStrength = Double

-- Implementation
                     
addAgent :: Agent -> ScentedTile -> ScentedTile
addAgent agent tile = tile { agents = Just agent }

addScent :: Agent -> Double -> ScentedTile -> ScentedTile
addScent agent strength tile = let scent = scents tile
                               in tile { scents = M.insertWith max (Scent agent) strength scent }

clearScent :: ScentedTile -> ScentedTile
clearScent tile = tile { scents = M.empty }

clearAgent :: ScentedTile -> ScentedTile
clearAgent tile = tile { agents = Nothing }

clearAgents :: ScentedWorld -> ScentedWorld
clearAgents w = w // [(p, clearAgent t) | (p,t) <- assocs w]

-- | initialize the world, setting the land type of each tile
initScentedWorld :: World -> ScentedWorld
initScentedWorld w = listArray (bounds w) [initTile $ tile t | (p,t) <- assocs w]

-- | Initialize a scented tile's landtype based on a Tile. (initialize if water or land)
initTile :: Tile -> ScentedTile
initTile tile =
  case tile of
    Ants.Water -> ScentedTile (Just Diffusion.Water) M.empty
    _ -> ScentedTile Nothing M.empty
  
-- | Place own ants and food, plus their scents,  on the scented world
-- NOTE: EDIT IF YOU WANT TO TRACK MORE AGENTS
placeAgents :: GameState -> ScentedWorld -> ScentedWorld
placeAgents gs = placeOwnHills . placeEnemyHills . placeOwnAnts . placeFood
  where
    placeFood = placeItem (food gs) Food
    placeOwnHills = placeItem (map pointHill $ myHills $ hills gs) OwnHill
    placeEnemyHills = placeItem (map pointHill $ enemyHills $ hills gs) EnemyHill
    placeEnemyAnts = placeItem (map pointAnt $ enemyAnts $ ants gs) EnemyAnt
    placeOwnAnts = placeItem (map pointAnt $ myAnts $ ants gs) OwnAnt

placeItem :: [Point] -> Agent -> ScentedWorld -> ScentedWorld
placeItem points agent w =
  w // [(p,t') | p <- points, let t' = addScent agent diff_max $ addAgent agent $ w%!p]