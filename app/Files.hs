{-# LANGUAGE LambdaCase #-}

module Files
  ( parseLayouts
  , parseSrc
  , build
  , saveFile
  , copyAssets
  )
where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text.IO         as TIO
import           Document
import           Macro
import           Parser
import           System.Directory
import           System.FilePath
import           Text.Megaparsec      (MonadParsec (eof), parse)
import           ToHtml

------------ "Building" files

-- | Applies a layout (i.e. a 'Macro') to a 'Document'
build :: [Macro] -> (FilePath, Document) -> (FilePath, Maybe [Content])
build layouts = fmap (applyLayout layouts)

-- | Writes the HTML of a list of 'Content' into a specific directory
saveFile :: FilePath -> (FilePath, Maybe [Content]) -> IO ()
saveFile _ (path, Nothing)      = do
  putStrLn ("(!) Something's wrong at " ++ path ++ ": maybe the layout doesn't exist?") >> pure ()
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
parseLayouts :: FilePath -> StateT Env IO [Either ParserError Macro]
parseLayouts path = do
  env <- get
  isFile <- liftIO (doesFileExist path)
  if isFile
    then parseMacro env >>= \case
          Left e  -> pure [Left e]
          Right m -> modify' (addMacroName (macroName m)) >> pure [Right m]
    else do
      ps <- liftIO (listDirectory path)
      concat <$> mapM (\p -> parseLayouts (path </> p)) ps
  where
    parseMacro = liftIO . parseWithEnv macro path

-- | Parses a 'Document' file
parseSrc :: FilePath -> ReaderT Env IO [(FilePath, Either ParserError Document)]
parseSrc path = do
  env <- ask
  isFile <- liftIO (doesFileExist path)
  if isFile
    then parseDocument env >>= \doc -> pure [(path, doc)]
    else do
      ps <- liftIO (listDirectory path)
      concat <$> mapM (\p -> parseSrc (path </> p)) ps
  where
    parseDocument = liftIO . parseWithEnv document path

------------ Utils

-- | Little utility that parses a file with a specific 'Parser' and 'Env'
parseWithEnv :: (Env -> Parser a) -> FilePath -> Env -> IO (Either ParserError a)
parseWithEnv parser path env = parse (parser env <* eof) path <$> TIO.readFile path

-- | Applies a function recursively to a directory
walkDir :: FilePath -> (FilePath -> IO a) -> IO [a]
walkDir path f = do
  isFile <- doesFileExist path
  if isFile
    then (: []) <$> f path
    else do
      ps <- listDirectory path
      concat <$> mapM (\p -> walkDir (path </> p) f) ps
