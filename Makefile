run:
	cabal run haskell-ssg -- _src

repl:
	cabal repl

cli:
	cabal repl app/Main.hs --ghc-options="-Wwarn"