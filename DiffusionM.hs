module DiffusionM(
  -- types
  Agent(..)
  , Scent
  , ScentedTile(..)
  , ScentedWorld(..)
    
    -- constants
  , diff_rate
  , diff_max
    
    -- functions
  , initScentedWorld
  , resetWorld
  , propagate
  , scent
  , getTile
  , neighboringPoints
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Map (Map)
import Data.List

import qualified Data.Array.Unboxed as U
import qualified Data.Map as M
import qualified Ants as Ants

import Ants hiding ((%!), (%!%),Water)
import Unexplored
  
-- CONSTANTS

diff_rate = 0.2

diff_max = 50

propagate_length = 11

min_turn_unseen = 9

-- TYPES

-- | Agents are items that release their own scent
data Agent = Water | Unexplored | Food | OwnAnt | EnemyAnt | OwnHill | EnemyHill
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

type ScentStack = Map Scent Double

type ScentedWorld = Array Point ScentedTile

type MWorld s = STArray s Point ScentedTile

-- Implementation
                     
addAgent :: Agent -> ScentedTile -> ScentedTile
addAgent agent tile = tile { agents = Just agent }

addScent :: Agent -> Double -> ScentedTile -> ScentedTile
addScent agent strength tile = let scent = scents tile
                               in tile { scents = M.insert (Scent agent) strength scent }

clearScent :: ScentedTile -> ScentedTile
clearScent tile = tile { scents = M.empty }

clearScents :: MWorld s -> ST s ()
clearScents w = do
  assocs <- getAssocs w
  forM_ assocs $ \(p,t) -> do
    writeArray w p $ clearScent t

clearAgent :: ScentedTile -> ScentedTile
clearAgent tile = tile { agents = Nothing }

clearAgents :: MWorld s -> ST s ()
clearAgents w = do 
  assocs <- getAssocs w
  forM_ assocs $ \(p,t) -> do
    t' <- return $ clearAgent t
    writeArray w p t'

-- | initialize the world, setting the land type of each tile
initScentedWorld :: World -> ScentedWorld
initScentedWorld w = listArray (bounds w) [initTile $ tile t | (p,t) <- assocs w]

resetWorld :: GameState -> Unexplored -> ScentedWorld -> ScentedWorld
resetWorld gs unex w = runSTArray $ do
  mworld <- thaw w
  clearAgents mworld
  placeAgents gs unex mworld
  return mworld

-- | Initialize a scented tile's landtype based on a Tile. (initialize if water or land)
initTile :: Tile -> ScentedTile
initTile tile =
  case tile of
    Ants.Water -> ScentedTile (Just DiffusionM.Water) M.empty
    _ -> ScentedTile Nothing M.empty
  
-- | Place own ants and food, plus their scents,  on the scented world
-- NOTE: EDIT IF YOU WANT TO TRACK MORE AGENTS
placeAgents :: GameState -> Unexplored -> MWorld s -> ST s ()
placeAgents gs unex mworld = do
  placeUnexplored mworld
  placeEnemyAnts mworld
  placeOwnAnts mworld 
  placeFood mworld
  placeEnemyHills mworld 
  where
    placeFood = placeItem (food gs) Food
    placeOwnHills = placeItem (map pointHill $ myHills $ hills gs) OwnHill
    placeEnemyHills = placeItem (map pointHill $ enemyHills $ hills gs) EnemyHill
    placeEnemyAnts = placeItem (map pointAnt $ enemyAnts $ ants gs) EnemyAnt
    placeOwnAnts = placeItem (map pointAnt $ myAnts $ ants gs) OwnAnt
    placeUnexplored = placeItem (map fst $ filter ((>=min_turn_unseen) . snd) $ U.assocs unex) Unexplored

placeItem :: [Point] -> Agent -> MWorld s -> ST s ()
placeItem points agent w = do
  forM_ points $ \p -> do
    t <- readArray w p
    t' <- return $ addScent agent diff_max $ addAgent agent t
    writeArray w p t'
     
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

-- | Returns all neighboring points that aren't water
neighboringPoints :: ScentedWorld -> Point -> [Point]
neighboringPoints w p = filter notWater $ map (flip move p) $ [North .. West]
  where
    notWater = not . (==(Just Water)) . agents . (w%!)
                        
lambda :: Maybe Agent -> Double
lambda Nothing = 1 -- land
lambda (Just agent) =
  M.findWithDefault 1 agent $ M.fromList [(Food, 1.1)
                                          , (OwnAnt, 0.5)
                                          , (Water, 0)
                                          , (EnemyAnt, 1.11)
                                          ]
    

-- | The diffusion equation used to compute the diffusion value of each tile
diffusion :: Point -> Agent -> ScentedWorld -> Double
diffusion point agent world = 
  let neighborTiles = map (world%!) $ neighboringPoints world point
      thisTile = world ! point
      diffusionvals = map (\x -> scent agent x - scent agent thisTile) neighborTiles
      summation = sum diffusionvals 
  in (lambda $ agents thisTile) * (scent agent thisTile + diff_rate * summation)
     
scent :: Agent -> ScentedTile -> Double
scent agent tile = M.findWithDefault 0 (Scent agent) $ scents tile

propagate :: World -> ScentedWorld -> ScentedWorld
propagate world w = runSTArray $ do
  mworld <- unsafeThaw w
  forM_ [1..propagate_length] $ \_ ->
    propagate1 world mworld
  return mworld

propagate1 :: World -> MWorld s -> ST s ()
propagate1 world modworld = do
  referenceworld <- freeze modworld
  points <- liftM (map fst) $ getAssocs modworld
  agents' <- return $ [Unexplored,Food,OwnAnt,EnemyAnt,EnemyHill]
  forM_ points $ \p -> do
    t <- return $ referenceworld ! p
    let addScent' :: ScentedTile -> Agent -> ScentedTile
        addScent' tile agent =
          case agents tile of
            Nothing -> addScent agent (diffusion p agent referenceworld) tile
            Just current_agent -> if current_agent == agent
                                  then addScent agent diff_max tile
                                  else addScent agent (diffusion p agent referenceworld) tile
        addScents' t = foldl addScent' t agents'
      in when (visible $ world ! p) 
         $ writeArray modworld p $ addScents' t

getTile :: Point -> ScentedWorld -> ScentedTile
getTile = flip (%!)