dist/build/gol/gol: src/GameOfLife/Game.hs
	cabal build

test: dist/build/gol/gol
	./dist/build/gol/gol
