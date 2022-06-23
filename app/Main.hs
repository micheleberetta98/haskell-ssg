{-# LANGUAGE MultiWayIf #-}

module Main where

import           Building
import           Control.Exception
import           Document
import           File
import           Opts
import           Parser
import           Parsing
import           Server
import           System.IO

type WithError = Either ParserError

-- | Custom data types for parsing and building errors
data Errors = NoParse [ParserError] | BuildErrors [BuildError]
  deriving (Show)

instance Exception Errors

main :: IO ()
main =
  parseAndBuild
  >> getOpts
  >>= serve . buildFolder
  >>= loop

parseAndBuild :: IO ()
parseAndBuild = do
  parse
    >>= buildFiles
    >>= saveFiles
  `catch` \case
    NoParse es     -> prettyPrintErrors es prettyParserError
    BuildErrors es -> prettyPrintErrors es show

loop :: Server -> IO ()
loop s = do
  putStr "> "
  hFlush stdout
  cmd <- getLine
  if | cmd `elem` ["q", "quit"]   -> kill s >> pure ()
     | cmd `elem` ["r", "reload"] -> parseAndBuild >> reload s >>= loop
     | cmd `elem` ["h", "help"]   -> putStrLn "Available commands: r/reload, q/quit, h/help" >> loop s
     | otherwise                  -> putStrLn "No such command: use h or help" >> loop s

saveFiles :: [File [Content]]  -> IO ()
saveFiles docs = do
  output <- buildFolder <$> getOpts
  assets <- staticFolder <$> getOpts
  outputAssets <- outputStaticFolder <$> getOpts
  putStrLn "Building..."
  mapM_ (renderFile output) docs
  copyDir assets outputAssets

prettyPrintErrors :: [a] -> (a -> String) -> IO ()
prettyPrintErrors es toStr = mapM_ (hPutStrLn stderr . toStr) es
