module Main where

import           Control.Monad
import           Data.Either
import           Files
import           Server
import           System.Environment
import           Text.Megaparsec    (errorBundlePretty)

layoutsDir, srcDir, buildDir, assetsDir, outputAssetsDir :: FilePath
layoutsDir = "_layouts"
srcDir     = "_src"
buildDir   = "_build"
assetsDir  = "_static"
outputAssetsDir = "_build/static"

main :: IO ()
main = do
  putStrLn $ "Reading layouts at " ++ layoutsDir ++ ".. "
  layouts <- parseLayouts layoutsDir
  putStrLn $ "Reading src at " ++ layoutsDir ++ ".. "
  srcFiles <- parseSrc srcDir
  putStrLn $ "Building into " ++ buildDir ++ ".. "

  let layouts' = rights layouts
  forM_ srcFiles $ \(path, eDoc) -> do
    case eDoc of
      Left err  -> putStrLn (errorBundlePretty err)
      Right doc -> saveFile buildDir (build layouts' (path, doc))

  putStrLn "Copying assets dir..."
  copyAssets assetsDir outputAssetsDir

  port <- getPort 4000
  serve buildDir port


getPort :: Int -> IO Int
getPort defaultPort = maybe defaultPort read <$> lookupEnv "PORT"

