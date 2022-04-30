module Main where

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
  parse
    >>= buildFiles
    >>= saveFiles

  getOpts >>= serve . buildFolder

------------ Parsing

parse :: IO ([Layout], [Macro], [File Document])
parse = do
  (layouts, macros, docs) <- getMacrosAndDocuments
  let (macroErrors, macros')   = separateErrors macros
      (layoutErrors, layouts') = separateErrors layouts
      (fileErrors, docs')      = separateErrors docs

  panicIfErrors (macroErrors <> layoutErrors <> fileErrors) errorBundlePretty
  pure (layouts', macros', docs')

getMacrosAndDocuments :: IO ([WithError Layout], [WithError Macro], [WithError (File Document)])
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
  panicIfErrors buildErrors show
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

panicIfErrors :: [a] -> (a -> String) -> IO ()
panicIfErrors [] _     = pure ()
panicIfErrors es toStr = do
  mapM_ (hPutStrLn stderr . toStr) es
  exitFailure

separateErrors :: [Either a b] -> ([a], [b])
separateErrors = bimap reverse reverse . foldl' accum ([], [])
  where
    accum (es, xs) (Left e)  = (e : es, xs)
    accum (es, xs) (Right x) = (es, x : xs)
