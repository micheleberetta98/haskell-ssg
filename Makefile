run:
	cabal run haskell-ssg

repl:
	cabal repl

cli:
	cabal repl app/Main.hs --ghc-options="-Wwarn"