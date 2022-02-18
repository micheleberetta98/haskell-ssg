module Files
  ( parseLayouts
  , parseSrc
  , build
  , saveFile
  , copyAssets
  )
where

import           Control.Monad
import qualified Data.Text.IO     as TIO
import           Document
import           Macro
import           Parser
import           System.Directory
import           System.FilePath
import           Text.Megaparsec
import           ToHtml

------------ "Building" files

-- | Applies a layout (i.e. a 'Macro') to a 'Document'
build :: [Macro] -> (FilePath, Document) -> (FilePath, Maybe [Content])
build layouts = fmap (applyLayout layouts)

-- | Writes the HTML of a list of 'Content' into a specific directory
saveFile :: FilePath -> (FilePath, Maybe [Content]) -> IO ()
saveFile _ (path, Nothing) = putStrLn ("(!) Something's wrong at " ++ path ++ ": maybe the layout doesn't exist?") >> pure ()
saveFile dir (path, Just stuff) = do
  createDirectoryIfMissing True (takeDirectory path')
  TIO.writeFile path' (render $ toHtml stuff)
  where
    path' = deriveNewPath path
    deriveNewPath = joinPath . swapRootDir . splitDirectories . flip replaceExtension "html"
    swapRootDir []           = []
    swapRootDir ("." : rest) = "." : swapRootDir rest
    swapRootDir (_ : rest)   = dir : rest

-- | Copies a folder from one path to another
copyAssets :: FilePath -> FilePath -> IO ()
copyAssets from to = void $ walkDir from $ \p -> do
    let path' = takeDirectory (dropRootDir p)
    createDirectoryIfMissing True (to </> path')
    copyFile p (to </> path' </> takeFileName p)
  where
    dropRootDir = joinPath . drop 1 . splitDirectories

------------ Parsing of layouts and src files

-- | Parses a layout file (a 'Macro')
parseLayouts :: FilePath -> IO [Either ParserError Macro]
parseLayouts path = map snd <$> parsePath path (macro <* eof)

-- | Parses a 'Document' file
parseSrc :: FilePath -> IO [(FilePath, Either ParserError Document)]
parseSrc path = parsePath path (document <* eof)

------------ Utils

-- | Generic parsing of a path, file or directory
parsePath :: FilePath -> Parser a -> IO [(FilePath, Either ParserError a)]
parsePath path p = do
  isFile <- doesFileExist path
  (if isFile then parseFile else parseDir) path p

-- | Parses a whole directory recursively with the parser @p@
parseDir :: FilePath -> Parser a -> IO [(FilePath, Either ParserError a)]
parseDir dir p = do
  paths <- listDirectory dir
  concat <$> mapM parse' paths
  where parse' path = parsePath (dir </> path) p

-- | Parses a single file with the parser @p@
parseFile :: FilePath -> Parser a -> IO [(FilePath, Either ParserError a)]
parseFile path p = do
  result <- parse p path <$> TIO.readFile path
  pure [(path, result)]

-- | Applies a function recursively to a directory
walkDir :: FilePath -> (FilePath -> IO a) -> IO [a]
walkDir path f = do
  isFile <- doesFileExist path
  if isFile
    then (: []) <$> f path
    else do
      ps <- listDirectory path
      concat <$> mapM (\p -> walkDir (path </> p) f) ps
