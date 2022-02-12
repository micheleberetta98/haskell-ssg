module Files
  ( parseLayouts
  , parseSrc
  , build
  , saveFile
  )
where

import qualified Data.Text.IO     as TIO
import           Document
import           Macro
import           Parser
import           System.Directory
import           System.FilePath
import           Text.Megaparsec
import           ToHTML

------------ "Building" files

build :: [Macro] -> (FilePath, Document) -> (FilePath, Maybe [Content])
build layouts = fmap (applyLayout layouts)

saveFile :: FilePath -> (FilePath, Maybe [Content]) -> IO ()
saveFile _ (path, Nothing) = putStrLn ("(!) Something's wrong at " ++ path ++ ": maybe the layout doesn't exist?") >> pure ()
saveFile dir (path, Just stuff) =
  let path' = deriveNewPath path
  in do
    createDirectoryIfMissing True (takeDirectory path')
    TIO.writeFile path' (toHTML stuff)
  where
    deriveNewPath = joinPath . swapRootDir . splitDirectories . flip replaceExtension "html"
    swapRootDir []           = []
    swapRootDir ("." : rest) = "." : swapRootDir rest
    swapRootDir (_ : rest)   = dir : rest

------------ Parsing of layouts and src files

parseLayouts :: FilePath -> IO [Either ParserError Macro]
parseLayouts path = map snd <$> parsePath path (macro <* eof)

parseSrc :: FilePath -> IO [(FilePath, Either ParserError Document)]
parseSrc path = parsePath path (document <* eof)

------------ Utils

parsePath :: FilePath -> Parser a -> IO [(FilePath, Either ParserError a)]
parsePath path p = do
  isFile <- doesFileExist path
  (if isFile then parseFile else parseDir) path p

parseDir :: FilePath -> Parser a -> IO [(FilePath, Either ParserError a)]
parseDir dir p = do
  paths <- listDirectory dir
  concat <$> mapM parse' paths
  where parse' path = parsePath (dir </> path) p

parseFile :: FilePath -> Parser a -> IO [(FilePath, Either ParserError a)]
parseFile path p = do
  result <- parse p path <$> TIO.readFile path
  pure [(path, result)]
