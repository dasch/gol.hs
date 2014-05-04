dist/build/gol/gol: src/GameOfLife/*.hs
	cabal build

test: dist/build/gol/gol
	./dist/build/gol/gol
