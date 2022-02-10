module Files
  ( parseLayouts
  , parseSrc
  )
where

import           Document
import           Macro
import           Parser
import           System.Directory
import           System.FilePath

------------ Parsing of layouts and src files

parseLayouts :: FilePath -> IO [Either ParserError Macro]
parseLayouts path = map snd <$> parsePath path macro

parseSrc :: FilePath -> IO [(FilePath, Either ParserError Document)]
parseSrc path = parsePath path document

------------ Utils

parsePath :: FilePath -> Parser a -> IO [(FilePath, Either ParserError a)]
parsePath path p = do
  isFile <- doesFileExist path
  (if isFile then parseSingleFile else parseDir) path p

parseDir :: FilePath -> Parser a -> IO [(FilePath, Either ParserError a)]
parseDir dir p = do
  paths <- listDirectory dir
  concat <$> mapM parse' paths
  where parse' path = parsePath (dir </> path) p

parseSingleFile :: FilePath -> Parser a -> IO [(FilePath, Either ParserError a)]
parseSingleFile path p = do
  res <- parseFile path p
  pure [(path, res)]
