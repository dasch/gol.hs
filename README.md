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


###### The World


The World in Game of Life is conceptually an infinite plane of cells, only some finite subset of which are alive. The game progresses by applying the rules of the game, yielding a new World with a potentially different set of live cells. This process is repeated indefinitely.

In this implementation the World is represented by a list of live cells, each identified by its position on the plane:

```haskell
data Cell = Cell Int Int
data World = World [Cell]
```

Since list processing is at the heart of languages like Haskell, this was a natural choice.
