module GameOfLife.Patterns where

import GameOfLife.World

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
