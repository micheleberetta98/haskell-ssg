module Main where

import           Files
import           System.Environment

layoutsDir, srcDir, buildDir :: FilePath
layoutsDir = "_layouts"
srcDir     = "_src"
buildDir   = "_build"

main :: IO ()
main = do
  putStrLn $ "Reading layouts at " ++ layoutsDir ++ ".. "
  layouts <- parseLayouts layoutsDir
  print layouts
  putStrLn $ "\nReading src at " ++ layoutsDir ++ ".. "
  srcFiles <- parseSrc srcDir
  print srcFiles
  putStrLn $ "\nBuilding into " ++ buildDir ++ ".. "
  pure ()


getPort :: Int -> IO Int
getPort defaultPort = maybe defaultPort read <$> lookupEnv "PORT"

