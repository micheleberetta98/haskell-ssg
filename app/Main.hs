{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Either
import           Files
import           Opts
import           Parser               (defaultEnv)
import           Server
import           Text.Megaparsec      (errorBundlePretty)

main :: IO ()
main = do
  -- opts <- getOpts
  let opts = Options
              { srcFolder = "_src"
              , layoutsFolder = "_layouts"
              , staticFolder = "_static"
              , buildFolder = "_build"
              , outputStaticFolder = "_build/static"
              }

  let layoutsDir   = layoutsFolder opts
      srcDir       = srcFolder opts
      buildDir     = buildFolder opts
      assetsDir    = staticFolder opts
      outAssetsDir = outputStaticFolder opts

  putStrLn   "-------------------------------------------"
  putStrLn $ "Reading layouts at " ++ layoutsDir ++ ".. "
  (layouts, env') <- runStateT (parseLayouts layoutsDir) defaultEnv
  forM_ layouts $ \case
      Left e -> putStrLn (errorBundlePretty e)
      _      -> pure ()

  putStrLn   "-------------------------------------------"
  putStrLn $ "Reading src at " ++ srcDir ++ ".. "
  srcFiles <- runReaderT (parseSrc srcDir) env'
  let layouts' = rights layouts
  forM_ srcFiles $ \(path, eDoc) -> do
    case eDoc of
      Left err  -> putStrLn (errorBundlePretty err)
      Right doc -> saveFile buildDir (build layouts' (path, doc))

  putStrLn   "-------------------------------------------"
  putStrLn $ "Building into " ++ buildDir ++ ".. "
  putStrLn "Copying assets dir..."
  copyAssets assetsDir outAssetsDir

  putStrLn   "-------------------------------------------"
  serve buildDir
