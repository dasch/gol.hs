compile: src/GameOfLife/*.hs
	cabal build

test: compile
	./dist/build/gol/gol

profile: compile
	./dist/build/gol-profile/gol-profile
