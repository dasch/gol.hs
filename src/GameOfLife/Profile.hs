import Control.Concurrent
import GameOfLife.World
import GameOfLife.Patterns
import GameOfLife.Utils
import GameOfLife.UI

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
        liveCells = cellsIn $ head $ drop 400 $ evolutions world
    in print $ length liveCells
