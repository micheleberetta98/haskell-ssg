# Haskell SSG

A *Static Site Generator*  written in Haskell.

## Compiling and running

You'll need *cabal* and *GHC 9.2.2*, both available at https://www.haskell.org/ghcup/.
Onse installed run:
* `make build` for building
* `make test` for running the tests
* `make run` for running the server at http://localhost:4000 with some defaults

You can specify the port of the server by setting the environment variable `PORT`.