module Main where

import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Foldable
import           Document            (Content, Document)
import           Files
import           Macro               (Layout, Macro)
import           Opts                (Options (..), getOpts)
import           Parser              (ParserError, defaultEnv)
import           Server              (serve)
import           System.Exit         (exitFailure)
import           System.IO           (hPutStrLn, stderr)
import           Text.Megaparsec     (errorBundlePretty)

type WithError = Either ParserError

main :: IO ()
main = do
  (layouts, macros, docs) <- getMacrosAndDocuments

  let (macroErrors, macros')   = separateErrors macros
      (layoutErrors, layouts') = separateErrors layouts
      (fileErrors, docs')      = separateErrors docs
      (buildErrors, finalDocs) = separateErrors $ map (build layouts' macros') docs'

      parserErrors = macroErrors <> layoutErrors <> fileErrors

  unless (null parserErrors) $ printParserErrors parserErrors >> exitFailure
  unless (null buildErrors)  $ printBuildErrors buildErrors >> exitFailure
  saveFiles finalDocs

  buildDir <- buildFolder <$> getOpts
  serve buildDir

getMacrosAndDocuments :: IO ([WithError Layout], [WithError Macro], [WithError (File Document)])
getMacrosAndDocuments = do
  opts <- getOpts
  putStrLn "Reading macros..."
  ((layouts, macros), env) <- getMacros opts
  putStrLn "Reading source files..."
  files <- evalStateT (parseSrc (srcFolder opts)) env
  pure (layouts, macros, files)
  where
    getMacros opts = flip runStateT defaultEnv $ do
      ls <- parseMacros (layoutsFolder opts)
      ms <- parseMacros (macrosFolder opts)
      pure (ls, ms)

saveFiles :: [File [Content]]  -> IO ()
saveFiles docs = do
  output <- buildFolder <$> getOpts
  assets <- staticFolder <$> getOpts
  outputAssets <- outputStaticFolder <$> getOpts
  putStrLn "Building..."
  mapM_ (save output) docs
  copyAssets assets outputAssets

printBuildErrors :: [BuildError] -> IO ()
printBuildErrors = printErrorsWith show

printParserErrors :: [ParserError] -> IO ()
printParserErrors = printErrorsWith errorBundlePretty

printErrorsWith :: (a -> String) -> [a] -> IO ()
printErrorsWith f =  mapM_ (hPutStrLn stderr . f)

separateErrors :: [Either a b] -> ([a], [b])
separateErrors = bimap reverse reverse . foldl' accum ([], [])
  where
    accum (es, xs) (Left e)  = (e : es, xs)
    accum (es, xs) (Right x) = (es, x : xs)
