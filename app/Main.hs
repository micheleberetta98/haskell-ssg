{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad
import           Data.Either
import           Files
import           Server
import           Text.Megaparsec (errorBundlePretty)

layoutsDir, srcDir, buildDir, assetsDir, outputAssetsDir :: FilePath
layoutsDir = "_layouts"
srcDir     = "_src"
buildDir   = "_build"
assetsDir  = "_static"
outputAssetsDir = "_build/static"

main :: IO ()
main = do
  putStrLn   "-------------------------------------------"
  putStrLn $ "Reading layouts at " ++ layoutsDir ++ ".. "
  layouts <- parseLayouts layoutsDir
  forM_ layouts $ \case
      Left e -> putStrLn (errorBundlePretty e)
      _      -> pure ()

  putStrLn   "-------------------------------------------"
  putStrLn $ "Reading src at " ++ layoutsDir ++ ".. "
  srcFiles <- parseSrc srcDir
  let layouts' = rights layouts
  forM_ srcFiles $ \(path, eDoc) -> do
    case eDoc of
      Left err  -> putStrLn (errorBundlePretty err)
      Right doc -> saveFile buildDir (build layouts' (path, doc))

  putStrLn   "-------------------------------------------"
  putStrLn $ "Building into " ++ buildDir ++ ".. "
  putStrLn "Copying assets dir..."
  copyAssets assetsDir outputAssetsDir

  putStrLn   "-------------------------------------------"
  serve buildDir
