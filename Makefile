test: cabal-test

build:
	cabal build --ghc-options="-threaded -rtsopts -with-rtsopts=-N -O2"

run:
	cabal run haskell-ssg -- _src

repl:
	cabal repl

cli:
	cabal repl app/Main.hs

cabal-test:
	cabal test --test-show-details=direct