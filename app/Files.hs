{-# LANGUAGE FlexibleContexts #-}

{-|
  Module      : Files
  Description : Source file management

  This modules deals with copying and building source files, which can be macros, documents
  or some static assets.
-}
module Files
  ( File
  , BuildError
  , parseMacros
  , parseSrc
  , build
  , save
  , copyAssets
  )
where

import           Control.Monad.State
import qualified Data.Text.IO        as T
import           Document
import           Macro
import           Parser
import           System.Directory
import           System.FilePath
import           ToHtml

------------ Types and instances

-- | A type to track a 'FilePath' along some content
data File a = File FilePath a

-- | Some errors can appear when building files, such as a layout or a 'Macro' missing
newtype BuildError = NoLayoutFound FilePath

instance Show BuildError where
  show (NoLayoutFound p) = concat ["(!) Something went wrong at ", p, ": maybe the layout doesn't exist?"]

------------ "Building" files

-- | Applies a layout (i.e. a 'Macro') to a 'Document'
build :: [Layout] -> [Macro] -> File Document -> Either BuildError (File [Content])
build layouts macros (File p doc) = File p <$> toEither (applyMacros doc)
  where
    applyMacros = fmap (expandAll macros) . applyLayout layouts
    toEither (Just x) = Right x
    toEither Nothing  = Left (NoLayoutFound p)

-- | Writes the HTML of a list of 'Content' into a specific directory
save :: FilePath -> File [Content] -> IO ()
save dir (File path stuff) = do
  createDirectoryIfMissing True (takeDirectory path')
  T.writeFile path' (render $ toHtml stuff)
  where
    path' = deriveNewPath path
    deriveNewPath = joinPath . swapRootDir . splitDirectories . flip replaceExtension "html"
    swapRootDir []           = []
    swapRootDir ("." : rest) = "." : swapRootDir rest
    swapRootDir (_ : rest)   = dir : rest

-- | Copies a folder from one path to another
copyAssets :: FilePath -> FilePath -> IO ()
copyAssets from to = do
  isFile <- doesFileExist from
  if isFile
    then do
      let path' = to </> takeDirectory (dropRootDir from)
      createDirectoryIfMissing True path'
      copyFile from (path' </> takeFileName from)
    else
      listDirectory from >>= mapM_ copySubDir
  where
    copySubDir d = copyAssets (from </> d) to
    dropRootDir = joinPath . drop 1 . splitDirectories

------------ Parsing of layouts and src files

-- | Parses all 'Macro's in a directory
parseMacros :: FilePath -> StateT Env IO [Either ParserError Macro]
parseMacros path = parseDir macro path $ const $ \x -> pure [x]

-- | Parses all 'Document's in a directory
parseSrc :: FilePath -> StateT Env IO [Either ParserError (File Document)]
parseSrc path = parseDir document path $ \p doc -> pure [File p <$> doc]

------------ Utils

-- | Runs a parser against all files in a directory (subdirectories included),
-- also allowing to keep an environment across all calls
parseDir :: Parser a                                                    -- ^ A function to get the parser given an environment
            -> FilePath                                                 -- ^ The directory containing the files
            -> (FilePath -> Either ParserError a -> StateT Env IO [b])  -- ^ What to do with the parser result on a single file
            -> StateT Env IO [b]
parseDir parser path k = do
  isFile <- liftIO (doesFileExist path)
  if isFile
    then parse parser path >>= k path
    else do
      ps <- liftIO (listDirectory path)
      concat <$> mapM parseSubDir ps
  where
    parseSubDir p = parseDir parser (path </> p) k

-- | Little utility that parses a file with a specific 'Parser' and 'Env'
-- parse :: Parser a -> FilePath -> IO (Either ParserError a)
parse :: Parser a -> FilePath -> StateT Env IO (Either ParserError a)
parse parser path = lift (T.readFile path) >>= runParser parser path
