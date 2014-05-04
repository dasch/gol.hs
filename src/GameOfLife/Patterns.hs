module GameOfLife.Patterns where

import GameOfLife.Parser

blinker = parsePattern ["@ @ @"]

glider = parsePattern [". @ .",
                       ". . @",
                       "@ @ @"]

toad = parsePattern [". @ @ @",
                     "@ @ @ ."]

beacon = parsePattern ["@ @ . .",
                       "@ @ . .",
                       ". . @ @",
                       ". . @ @"]
