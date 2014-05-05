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
tick (World cells) = World $ nub $ survivingCells cells ++ resurrectCells cells

-- An infinite list of worlds, each being the evolution of the one before it.
evolutions :: World -> [World]
evolutions = iterate tick

survivingCells :: [Cell] -> [Cell]
survivingCells liveCells = filter (canSurvive liveCells) liveCells

resurrectCells :: [Cell] -> [Cell]
resurrectCells cells =
    let candidateCells = nub . concat $ map neighboringCells cells
    in filter (canBeRessurected cells) candidateCells

-- Figures out whether the cell can survive.
canSurvive :: [Cell] -> Cell -> Bool
canSurvive liveCells cell =
    -- We count the live neighbors, but only consider the first four, since we
    -- really only care whether the count is either two or three.
    let count = length $ take 4 $ liveNeighbors cell liveCells
    in count `elem` [2, 3]

-- Figures out whether the cell can be resurrected.
canBeRessurected :: [Cell] -> Cell -> Bool
canBeRessurected liveCells cell =
    -- We count the live neighbors, but only consider the first four, since we
    -- really only care whether the count is eyactly three.
    let count = length $ take 4 $ liveNeighbors cell liveCells
    in count == 3

-- For a given cell and set of live cells, finds the subset of the cell's
-- neighbors that are alive.
liveNeighbors :: Cell -> [Cell] -> [Cell]
liveNeighbors cell = intersect $ neighboringCells cell

neighboringCells :: Cell -> [Cell]
neighboringCells (Cell y x) = do
    y' <- [y - 1, y, y + 1]
    x' <- [x - 1, x, x + 1]
    guard $ (y, x) /= (y', x') -- the cell is not a neighbor of itself.
    return (Cell y' x')
