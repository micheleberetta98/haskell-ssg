module File where

import           Control.Monad.Trans
import           Data.Text           (Text)
import qualified Data.Text.IO        as T
import           System.Directory
import           System.FilePath

-- | A type to track a 'FilePath' along some content
data File a = File FilePath a

-- | Runs a function against all files in a directory (subdirectories included),
-- also allowing to keep an environment across all calls
runOnDir :: (Monad (t IO), MonadTrans t) =>
            (FilePath -> t IO a)                -- ^ A function to run on every file inside the directory
            -> FilePath                         -- ^ The directory to analyze
            -> (FilePath -> a -> t IO [b])      -- ^ A continuation function, what to do with the result of the custom function
            -> t IO [b]
runOnDir f basePath k = do
  isFile <- lift (doesFileExist basePath)
  if isFile
    then f basePath >>= k basePath
    else do
      ps <- lift (listDirectory basePath)
      concat <$> mapM runOnSubdir ps
  where
    runOnSubdir path = runOnDir f (basePath </> path) k

-- | Copies a folder from one path to another
copyDir :: FilePath -> FilePath -> IO ()
copyDir from to = do
  isFile <- doesFileExist from
  if isFile
    then do
      let path' = to </> takeDirectory (dropRootDir from)
      createDirectoryIfMissing True path'
      copyFile from (path' </> takeFileName from)
    else
      listDirectory from >>= mapM_ copySubDir
  where
    copySubDir d = copyDir (from </> d) to
    dropRootDir = joinPath . drop 1 . splitDirectories

-- | Writes the content provided into a specific directory
save :: FilePath -> String -> File Text -> IO ()
save dir ext (File path stuff) = do
  createDirectoryIfMissing True (takeDirectory path')
  T.writeFile path' stuff
  where
    path' = deriveNewPath path
    deriveNewPath = joinPath . swapRootDir . splitDirectories . flip replaceExtension ext
    swapRootDir []           = []
    swapRootDir ("." : rest) = "." : swapRootDir rest
    swapRootDir (_ : rest)   = dir : rest
