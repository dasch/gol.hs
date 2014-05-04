module GameOfLife.World where

import Data.List
import Control.Monad

data Cell = Cell Int Int deriving (Show, Eq)
data World = World [Cell]

tick :: World -> World
tick world = mergeWorlds (liveCells world world) (resurrectCells world world)

evolutions :: World -> [World]
evolutions = iterate tick

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
    -- We count the live neighbors, but only consider the first four, since we
    -- really only care whether the count is either two or three.
    let count = length $ take 4 $ liveNeighbors cell world
    in count `elem` [2, 3]

canBeRessurected :: World -> Cell -> Bool
canBeRessurected world cell =
    -- We count the live neighbors, but only consider the first four, since we
    -- really only care whether the count is exactly three.
    let count = length $ take 4 $ liveNeighbors cell world
    in count == 3

liveNeighbors :: Cell -> World -> [Cell]
liveNeighbors cell (World liveCells) = filter (areNeighbors cell) liveCells

neighboringCells :: Cell -> [Cell]
neighboringCells (Cell x y) = do
    x' <- [x - 1, x, x + 1]
    y' <- [y - 1, y, y + 1]
    guard $ (x, y) /= (x', y') -- the cell is not a neighbor of itself.
    return (Cell x' y')

areNeighbors :: Cell -> Cell -> Bool
areNeighbors a b = a `elem` (neighboringCells b)
