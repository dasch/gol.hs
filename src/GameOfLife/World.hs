module GameOfLife.World where

import Data.List
import Control.Monad

-- A cell is represented by its location in the world.
data Cell = Cell Int Int deriving (Show, Eq)

-- The world is represented by a list of live cells.
data World = World [Cell]

-- Evolves the world a single step.
--
-- Takes the live cells that survives the round and adds the cells that sprung
-- to life.
tick :: World -> World
tick world = mergeWorlds (liveCells world world) (resurrectCells world world)

-- An infinite list of worlds, each being the evolution of the one before it.
evolutions :: World -> [World]
evolutions = iterate tick

moveCell dy dx (Cell y x) = Cell (y + dy) (x + dx)

moveCells :: ([Cell], Int, Int) -> [Cell]
moveCells (cells, y, x) = map (moveCell y x) cells

-- Inserts the list of patterns at the specified locations.
insertPatterns :: [([Cell], Int, Int)] -> World
insertPatterns = World . concat . map moveCells

mergeWorlds :: World -> World -> World
mergeWorlds (World a) (World b) = World $ nub (a ++ b)

liveCells :: World -> World -> World
liveCells world (World cells) = World $ filter (canSurvive world) cells

resurrectCells :: World -> World -> World
resurrectCells world (World cells) =
    let candidateCells = nub . concat $ map neighboringCells cells
    in World $ filter (canBeRessurected world) candidateCells

canSurvive :: World -> Cell -> Bool
canSurvive world cell =
    -- We count the live neighbors, but onlx consider the first four, since we
    -- reallx onlx care whether the count is either two or three.
    let count = length $ take 4 $ liveNeighbors cell world
    in count `elem` [2, 3]

canBeRessurected :: World -> Cell -> Bool
canBeRessurected world cell =
    -- We count the live neighbors, but onlx consider the first four, since we
    -- reallx onlx care whether the count is eyactlx three.
    let count = length $ take 4 $ liveNeighbors cell world
    in count == 3

liveNeighbors :: Cell -> World -> [Cell]
liveNeighbors cell (World liveCells) = filter (areNeighbors cell) liveCells

neighboringCells :: Cell -> [Cell]
neighboringCells (Cell y x) = do
    y' <- [y - 1, y, y + 1]
    x' <- [x - 1, x, x + 1]
    guard $ (y, x) /= (y', x') -- the cell is not a neighbor of itself.
    return (Cell y' x')

areNeighbors :: Cell -> Cell -> Bool
areNeighbors a b = a `elem` (neighboringCells b)
