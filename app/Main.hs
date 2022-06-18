{-# LANGUAGE MultiWayIf #-}

module Main where

import           Control.Exception   (Exception, catch, throw)
import           Control.Monad       (unless)
import           Control.Monad.State (MonadTrans (lift), evalStateT)
import           Data.Bifunctor      (Bifunctor (bimap))
import           Data.Foldable       (Foldable (foldl'))
import           Document            (Content, Document)
import           Files               (BuildError, File, build, copyAssets,
                                      parseMacros, parseSrc, save)
import           Macro               (Layout, Macro)
import           Opts                (Options (..), getOpts)
import           Parser              (ParserError, defaultEnv, prettifyError)
import           Server              (Server, kill, reload, serve)
import           System.IO           (hFlush, hPutStrLn, stderr, stdout)

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
  ( parse
    >>= buildFiles
    >>= saveFiles )
  `catch` \case
    NoParse es     -> prettyPrintErrors es prettifyError
    BuildErrors es -> prettyPrintErrors es show

loop :: Server -> IO ()
loop s = do
  putStr "> "
  hFlush stdout
  cmd <- getLine
  if | cmd `elem` ["q", "quit"]   -> kill s >> pure ()
     | cmd `elem` ["r", "reload"] -> parseAndBuild >> reload s >>= loop
     | otherwise                  -> putStrLn "No such command: try reload or quit" >> loop s

------------ Parsing

parse :: IO ([Layout], [Macro], [File Document])
parse = do
  (layouts, macros, docs) <- getMacrosAndDocuments
  let (macroErrors, macros')   = separateErrors macros
      (layoutErrors, layouts') = separateErrors layouts
      (fileErrors, docs')      = separateErrors docs

      errors = macroErrors <> layoutErrors <> fileErrors

  unless (null errors) $
    throw (NoParse errors)

  pure (concat layouts', concat macros', docs')

getMacrosAndDocuments :: IO ([WithError [Layout]], [WithError [Macro]], [WithError (File Document)])
getMacrosAndDocuments = flip evalStateT defaultEnv $ do
  opts <- lift getOpts
  lift (putStrLn "Reading macros...")
  ls <- parseMacros (layoutsFolder opts)
  ms <- parseMacros (macrosFolder opts)
  lift (putStrLn "Reading source files...")
  fs <- parseSrc (srcFolder opts)
  pure (ls, ms, fs)

------------ Building

buildFiles :: ([Layout], [Macro], [File Document]) -> IO [File [Content]]
buildFiles (layouts, macros, docs) = do
  let (buildErrors, finalDocs) = separateErrors $ map (build layouts macros) docs
  unless (null buildErrors) $
    throw (BuildErrors buildErrors)
  pure finalDocs

saveFiles :: [File [Content]]  -> IO ()
saveFiles docs = do
  output <- buildFolder <$> getOpts
  assets <- staticFolder <$> getOpts
  outputAssets <- outputStaticFolder <$> getOpts
  putStrLn "Building..."
  mapM_ (save output) docs
  copyAssets assets outputAssets

------------ Utilities

prettyPrintErrors :: [a] -> (a -> String) -> IO ()
prettyPrintErrors es toStr = mapM_ (hPutStrLn stderr . toStr) es

separateErrors :: [Either a b] -> ([a], [b])
separateErrors = bimap reverse reverse . foldl' accum ([], [])
  where
    accum (es, xs) (Left e)  = (e : es, xs)
    accum (es, xs) (Right x) = (es, x : xs)
