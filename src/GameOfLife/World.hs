module GameOfLife.World (
    newWorld,
    World,
    worldWithCells,
    inWorld,
    Cell (..),
    cellCount,
    cellsIn,
    evolutions
) where

import Data.List
import qualified Data.Set as Set
import Control.Monad

-- A cell is represented by its location in the world.
data Cell = Cell Int Int deriving (Show, Eq)

-- The world is represented by a list of live cells.
type World = Set.Set Cell

instance Ord Cell where
    Cell a b `compare` Cell a' b' = (a, b) `compare` (a', b')

newWorld :: World
newWorld = Set.empty

worldWithCells :: [Cell] -> World
worldWithCells = Set.fromList

cellsIn :: World -> [Cell]
cellsIn = Set.toList

inWorld :: Cell -> World -> Bool
inWorld = Set.member

-- Evolves the world a single step.
--
-- Takes the live cells that survives the round and adds the cells that sprung
-- to life.
tick :: World -> World
tick world = survivingCells world `Set.union` resurrectCells world

-- An infinite list of worlds, each being the evolution of the one before it.
evolutions :: World -> [World]
evolutions = iterate tick

cellCount :: World -> Int
cellCount = Set.size

survivingCells :: World -> World
survivingCells world = Set.filter (canSurvive world) world

resurrectCells :: World -> World
resurrectCells world =
    let candidateCells = Set.unions $ map neighboringCells $ cellsIn world
    in Set.filter (canBeRessurected world) candidateCells

-- Figures out whether the cell can survive.
canSurvive :: World -> Cell -> Bool
canSurvive world cell = neighborCount world cell `elem` [2, 3]

-- Figures out whether the cell can be resurrected.
canBeRessurected :: World -> Cell -> Bool
canBeRessurected world cell = neighborCount world cell == 3

neighborCount world cell = Set.size $ liveNeighbors cell world 

-- For a given cell and set of live cells, finds the subset of the cell's
-- neighbors that are alive.
liveNeighbors :: Cell -> World -> World
liveNeighbors cell = Set.intersection $ neighboringCells cell

neighboringCells :: Cell -> World
neighboringCells (Cell y x) = Set.fromList $ do
    y' <- [y - 1, y, y + 1]
    x' <- [x - 1, x, x + 1]
    guard $ (y, x) /= (y', x') -- the cell is not a neighbor of itself.
    return (Cell y' x')
