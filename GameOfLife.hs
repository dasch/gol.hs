import Data.List
import Control.Monad

data Cell = Cell Int Int deriving (Show, Eq)
type World = [Cell]

tick :: World -> World
tick world = liveCells world world ++ resurrectCells world world

liveCells :: World -> [Cell] -> World
liveCells world [] = []
liveCells world (cell : cells)
    | isAlive cell world = cell : liveCells world cells
    | otherwise = liveCells world cells

resurrectCells :: World -> [Cell] -> World
resurrectCells world cells =
    let candidateCells = nub . concat $ map neighboringCells cells
    in filter (canBeRessurected world) candidateCells

isAlive :: Cell -> World -> Bool
isAlive cell world =
    let count = neighborCount cell world
    in count `elem` [2, 3]

canBeRessurected :: World -> Cell -> Bool
canBeRessurected world cell =
    let count = neighborCount cell world
    in count == 3

neighborCount :: Cell -> World -> Int
neighborCount _ [] = 0
neighborCount cell (cell' : cells)
    | areNeighbors cell cell' = 1 + neighborCount cell cells
    | otherwise = neighborCount cell cells

neighboringCells :: Cell -> [Cell]
neighboringCells (Cell x y) = do
    x' <- [x - 1, x, x + 1]
    y' <- [y - 1, y, y + 1]
    guard (x' /= y')
    return (Cell x' y')

-- x x x
-- x   x
-- x x x
areNeighbors :: Cell -> Cell -> Bool
areNeighbors (Cell x y) (Cell x' y') =
    let dx = abs (x - x')
        dy = abs (y - y')
    in (dx == 1 && dy == 1) ||
       (dx == 0 && dy == 1) ||
       (dx == 1 && dy == 0)

-- . x .
-- . x x
main =
    let world = [Cell 1 2, Cell 2 2, Cell 2 3]
    in do
        print world
        print $ tick world
        print $ tick $ tick world
