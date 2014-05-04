import Data.List
import Control.Monad
import Control.Concurrent

data Cell = Cell Int Int deriving (Show, Eq)
data World = World [Cell]

instance Show World where
    show (World cells) =
        let
            cols = 90
            rows = 40
            show' cell = if cell `elem` cells then " @" else " ."
            window = do
                x <- [1..rows]
                y <- [1..cols]
                return (Cell x y)
        in concat . intercalate ["\n"] . chunksOf cols $ map show' window

tick :: World -> World
tick world = mergeWorlds (liveCells world world) (resurrectCells world world)

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
    guard (x /= x' || y /= y')
    return (Cell x' y')

areNeighbors :: Cell -> Cell -> Bool
areNeighbors a b = a `elem` (neighboringCells b)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = (take n l) : (chunksOf n (drop n l))
  | otherwise = error "Negative n"

clear = putStr "\ESC[2J"

printWorld world = do
    clear
    print world
    threadDelay 400000

evolutions :: World -> [World]
evolutions = iterate tick

-- x x x
blinker :: Int -> Int -> [Cell]
blinker x y = [Cell x y, Cell x (y + 1), Cell x (y + 2)]

-- . x .
-- . . x
-- x x x
glider x y = [Cell x (y + 1), Cell (x + 1) (y + 2), Cell (x + 2) y, Cell (x + 2) (y + 1), Cell (x + 2) (y + 2)]

-- . x x x
-- x x x .
toad x y = [Cell x (y + 1), Cell x (y + 2), Cell x (y + 3), Cell (x + 1) y, Cell (x + 1) (y + 1), Cell (x + 1) (y + 2)]

main =
    let world = World $ blinker 2 2 ++ glider 8 10 ++ toad 12 20
    in mapM printWorld $ evolutions world
