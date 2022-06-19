test: cabal-test
doc: cabal-haddock

build:
	cabal build

run:
	cabal run haskell-ssg -- _src

repl:
	cabal repl

cli:
	cabal repl app/Main.hs

cabal-test:
	cabal test --test-show-details=direct
cabal-haddock:
	cabal haddock