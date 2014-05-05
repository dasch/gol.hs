module GameOfLife.UI where

import GameOfLife.World
import Data.List

data Window = Window Int Int [Cell]

window :: Int -> Int -> (World -> String)
window rows cols = renderWindow $ createWindow rows cols

createWindow :: Int -> Int -> Window
createWindow rows cols = Window rows cols $ blockOfCells rows cols

renderWindow :: Window -> World -> String
renderWindow (Window rows cols windowCells) (World liveCells) =
    let renderedCells = map (renderCell liveCells) windowCells
        lines = chunksOf cols renderedCells
        content = concat . intercalate ["\n"] $ lines
    in "Live cells: " ++ show (length liveCells) ++ "\n\n" ++ content

renderCell :: [Cell] -> Cell -> String
renderCell liveCells cell = if cell `elem` liveCells then " @" else " ."

blockOfCells :: Int -> Int -> [Cell]
blockOfCells rows cols = do
    y <- [1..rows]
    x <- [1..cols]
    return (Cell y x)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = (take n l) : (chunksOf n (drop n l))
  | otherwise = error "Negative n"
