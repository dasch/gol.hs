module GameOfLife.Patterns where

import GameOfLife.World
import GameOfLife.Parser

-- y y y
blinker :: Int -> Int -> [Cell]
blinker y x = [Cell y x, Cell y (x + 1), Cell y (x + 2)]

-- . y .
-- . . y
-- y y y
glider y x = [Cell y (x + 1), Cell (y + 1) (x + 2), Cell (y + 2) x, Cell (y + 2) (x + 1), Cell (y + 2) (x + 2)]

-- . y y y
-- y y y .
toad y x = [Cell y (x + 1), Cell y (x + 2), Cell y (x + 3), Cell (y + 1) x, Cell (y + 1) (x + 1), Cell (y + 1) (x + 2)]

beacon y x =
    let pattern = ["@ @ . .",
                   "@ @ . .",
                   ". . @ @",
                   ". . @ @"]
    in map (moveCell y x) $ parsePattern pattern
