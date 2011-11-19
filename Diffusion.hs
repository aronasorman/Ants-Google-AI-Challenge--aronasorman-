module Diffusion(
                -- types
                Agent
                , Scent
                , AgentStack
                , ScentStack
                , ScentedTile(..)
                , ScentedWorld
                  
                
                -- functions
                , processWorld
                  
                -- constants
                , diff_max
                , diff_min
                ) where

import Data.Array (assocs)
import Data.List
import Data.Map (Map)

import qualified Data.Map as M

import Ants

-- CONSTANTS

diff_rate = 0.25

diff_max = 10 :: Double

diff_min = 0.001

-- TYPES

-- | Agents are items that release their own scent
data Agent = Food | Own | Enemy | OwnHill | EnemyHill
           deriving (Eq, Ord, Show)

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

type ScentStack = Map Scent Double

type ScentedWorld = Map Point ScentedTile

-- exposed functions
                    
-- | Propagate the agents in the gamestate and and return a world with their scents
-- | scattered in the world
processWorld :: GameState -> ScentedWorld
processWorld gs = propagateAll gs $ placeAgents gs $ initScentedWorld (world gs)

---
--- Implementation
---
    
addAgent :: Agent -> ScentedTile -> ScentedTile
addAgent agent tile = let others = agents tile
                      in tile { agents = Just agent }

addScent :: Agent -> Double -> ScentedTile -> ScentedTile
addScent agent strength tile = let scent = scents tile
                               in tile { scents = M.insert (Scent agent) strength scent }

scentStrength :: Agent -> ScentedTile -> Double
scentStrength a = M.findWithDefault 0 (Scent a) . scents

clearScents :: ScentedTile -> ScentedTile
clearScents tile = tile { scents = M.empty }

-- | Initialize a scented tile's landtype based on a Tile. (initialize if water or land)
initTile :: Tile -> ScentedTile
initTile tile =
  case tile of
    Water -> ScentedTile Nothing WaterTile M.empty
    _ -> ScentedTile Nothing LandTile M.empty

-- | initialize the world, setting the land type of each tile
initScentedWorld :: World -> ScentedWorld
initScentedWorld w = M.fromList $ map scentedtile $ assocs w
  where
    scentedtile (bound,metatile) = (,) bound $ initTile . tile $ metatile

-- | Place own ants and food on the scented world
-- NOTE: EDIT IF YOU WANT TO TRACK MORE AGENTS
placeAgents :: GameState -> ScentedWorld -> ScentedWorld
placeAgents gs = placeOwnAnts . placeFood
  where
    placeFood = placeItem (food gs) Food
    placeOwnAnts = placeItem (map point $ myAnts $ ants gs) Own
    placeItem :: [Point] -> Agent -> ScentedWorld -> ScentedWorld
    placeItem [] _ = id
    placeItem (x:xs) agent = placeItem xs agent . M.adjust (addAgent agent) x

colBound :: ScentedWorld -> Col
colBound = col . fst . M.findMax

rowBound :: ScentedWorld -> Row
rowBound = row . fst . M.findMax

-- Takes the modulus of the indices before accessing the array
(%!) :: ScentedWorld -> Point -> ScentedTile
(%!) w p = M.findWithDefault undefined (w %!% p) w

(%!%) :: ScentedWorld -> Point -> Point
(%!%) w p = 
  let modCol = 1 + colBound w
      modRow = 1 + rowBound w
      ixCol  = col p `mod` modCol
      ixRow  = row p `mod` modRow
  in (ixRow, ixCol)

isWater :: Point -> ScentedWorld -> Bool
isWater p = (==WaterTile) . landtype . (%!p)

-- | Returns all neighboring points that aren't water
neighboringPoints :: ScentedWorld -> Point -> [Point]
neighboringPoints w p = filter (not . flip isWater w) . map (flip move p) $ [North .. West]

-- | Propagate all agent scents (own ants and food for now) throughout the world
-- NOTE: EDIT IF YOU WANT TO PROPAGATE MORE SCENTS
propagateAll :: GameState -> ScentedWorld -> ScentedWorld
propagateAll gs world = propagateMultiple points world
  where
    points = map point (myAnts $ ants gs) ++ food gs

-- | Given a list of agent locations and the world, propagate the
-- | scent of each agent throughout the world
propagateMultiple :: [Point] -> ScentedWorld -> ScentedWorld
propagateMultiple points w = foldr propagateAgentScent w points

-- | Propagate the scent of a single agent throughout the world
propagateAgentScent :: Point -> ScentedWorld -> ScentedWorld
propagateAgentScent point world =
  let tile = world %! point
      neighbors = neighboringPoints world point
  in case agents tile of
    Nothing -> world
    Just agent -> let addedAgentScentWorld = M.adjust (addScent agent diff_max) point world
                  in foldr (propagateScent agent $ diff_rate * diff_max) addedAgentScentWorld neighbors

propagateScent :: Agent -> Double -> Point -> ScentedWorld -> ScentedWorld
propagateScent agent diffVal point world =
  if diffVal < diff_min
  then world
  else let neighbors = neighboringPoints world point
           addedScentWorld = M.adjust (addScent agent diffVal) point world
       in foldr (propagateScent agent $ diffVal * diff_rate) addedScentWorld neighbors