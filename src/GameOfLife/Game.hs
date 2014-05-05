import Control.Concurrent
import GameOfLife.World
import GameOfLife.Patterns
import GameOfLife.Utils
import GameOfLife.UI

clear = putStr "\ESC[2J"

updateScreen content = do
    clear
    putStrLn content
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
    in mapM updateScreen $ map (window 40 90) $ evolutions world
