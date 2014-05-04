import Data.List
import Control.Monad
import Control.Concurrent

data Cell = Cell Int Int deriving (Show, Eq)
data World = World [Cell]

instance Show World where
    show (World cells) =
        let
            show' cell = if cell `elem` cells then " x" else " ."
            window = do
                x <- [1..10]
                y <- [1..10]
                return (Cell x y)
        in concat . intercalate ["\n"] . chunksOf 10 $ map show' window

tick :: World -> World
tick world = mergeWorlds (liveCells world world) (resurrectCells world world)

mergeWorlds :: World -> World -> World
mergeWorlds (World a) (World b) = World (a ++ b)

liveCells :: World -> World -> World
liveCells world (World cells) = World $ filter (isAlive world) cells

resurrectCells :: World -> World -> World
resurrectCells world (World cells) =
    let candidateCells = nub . concat $ map neighboringCells cells
    in World $ filter (canBeRessurected world) candidateCells

isAlive :: World -> Cell -> Bool
isAlive world cell =
    let count = neighborCount cell world
    in count `elem` [2, 3]

canBeRessurected :: World -> Cell -> Bool
canBeRessurected world cell =
    let count = neighborCount cell world
    in count == 3

neighborCount :: Cell -> World -> Int
neighborCount _ (World []) = 0
neighborCount cell (World (cell' : cells))
    | areNeighbors cell cell' = 1 + neighborCount cell (World cells)
    | otherwise = neighborCount cell (World cells)

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
    threadDelay 200000

evolutions :: World -> [World]
evolutions = iterate tick

main =
    let world = World [Cell 2 2, Cell 2 3, Cell 2 4]
    in mapM printWorld $ evolutions world
