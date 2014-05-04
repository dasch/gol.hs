import Data.List
import Control.Concurrent
import GameOfLife.World
import GameOfLife.Patterns

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
    let world = World $ blinker 2 2 ++ glider 8 10 ++ toad 12 20
    in mapM printWorld $ evolutions world
