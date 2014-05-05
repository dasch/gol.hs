import Data.List
import Control.Concurrent
import GameOfLife.World
import GameOfLife.Patterns
import GameOfLife.Utils

instance Show World where
    show (World cells) =
        let
            cols = 90
            rows = 40
            window = createWindow rows cols
            show' cell = if cell `elem` cells then " @" else " ."
        in concat . intercalate ["\n"] . chunksOf cols $ map show' window

createWindow rows cols = do
    y <- [1..rows]
    x <- [1..cols]
    return (Cell y x)

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

main =
    let patterns = [
            (blinker, 2, 2),
            (glider, 8, 10),
            (glider, 8, 50),
            (glider, 8, 60),
            (glider, 8, 70),
            (toad, 12, 20),
            (beacon, 6, 30),
            (acorn, 30, 50)]
        world = insertPatterns patterns
    in mapM printWorld $ evolutions world
