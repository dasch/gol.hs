module GameOfLife.Utils where

import GameOfLife.World

moveCell dy dx (Cell y x) = Cell (y + dy) (x + dx)

moveCells :: ([Cell], Int, Int) -> [Cell]
moveCells (cells, y, x) = map (moveCell y x) cells

-- Inserts the list of patterns at the specified locations.
insertPatterns :: [([Cell], Int, Int)] -> World
insertPatterns = World . concat . map moveCells
