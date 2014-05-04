module GameOfLife.Parser where

import GameOfLife.World

parsePattern :: [String] -> [Cell]
parsePattern pattern = concat . map parseLine $ zip pattern [1..]

parseLine :: (String, Int) -> [Cell]
parseLine (line, y) = map (Cell y) (liveCellIndeces 1 line)

liveCellIndeces :: Int -> [Char] -> [Int]
liveCellIndeces x [] = []
liveCellIndeces x (' ' : rest) = liveCellIndeces x rest
liveCellIndeces x ('.' : rest) = liveCellIndeces (x + 1) rest
liveCellIndeces x ('@' : rest) = x : liveCellIndeces (x + 1) rest
