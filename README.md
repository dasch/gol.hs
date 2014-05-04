gol.hs
======

Conway's Game of Life, implemented in Haskell!

This is my attempt at learning some Haskell while simultaneously kicking the tires of Life. The program will evolve an initial configuration indefinitely, updating the terminal on each tick.


#### How to run

Clone the repo and run `make test` in the directory.


#### Design

The program is structured into several modules:

* `GameOfLife.World` implements the world of GoL.
* `GameOfLife.Patterns` contains commonly used "patterns" of cells.
* `GameOfLife.Parser` contains functions that allow parsing a textual representation of a set of cells.
* `GameOfLife.Game` contains the actual executable part of the game.
