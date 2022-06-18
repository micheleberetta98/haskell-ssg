{-|
  Module      : Parsing
  Description : Parsing of source files

  This modules deals with parsing source files and building all the internal
  representations.
-}
module Parsing (parse, NoParse) where

import           Control.Exception
import           Control.Monad.State
import           Data.Bifunctor
import           Data.List
import qualified Data.Text.IO        as T
import           Document
import           File
import           Macro
import           Opts
import           Parser

type WithError = Either ParserError

newtype NoParse = NoParse [ParserError]
  deriving (Show)

instance Exception NoParse

-- | IO action that parses all layouts, macros and documents
parse :: IO ([Layout], [Macro], [File Document])
parse = do
  (ls, ms, docs) <- getMacrosAndDocuments
  let (macroErrors, ms')  = separateErrors ms
      (layoutErrors, ls') = separateErrors ls
      (fileErrors, docs') = separateErrors docs

      errors = macroErrors <> layoutErrors <> fileErrors

  unless (null errors) $
    throw (NoParse errors)

  pure (concat ls', concat ms', docs')

-- | Parses all layouts, macros and documents from the corresponding folders given by 'getOpts'
getMacrosAndDocuments :: IO ([WithError [Layout]], [WithError [Macro]], [WithError (File Document)])
getMacrosAndDocuments = flip evalStateT defaultEnv $ do
  opts <- lift getOpts
  lift (putStrLn "Reading macros...")
  ls <- parseMacros (layoutsFolder opts)
  ms <- parseMacros (macrosFolder opts)
  lift (putStrLn "Reading source files...")
  fs <- parseSrc (srcFolder opts)
  pure (ls, ms, fs)

-- | Parses all 'Macro's in a directory
parseMacros :: FilePath -> StateT Env IO [Either ParserError [Macro]]
parseMacros path = parseDir macros path $ const $ \x -> pure [x]

-- | Parses all 'Document's in a directory
parseSrc :: FilePath -> StateT Env IO [Either ParserError (File Document)]
parseSrc path = parseDir document path $ \p doc -> pure [File p <$> doc]

------------ Utilities

separateErrors :: [Either a b] -> ([a], [b])
separateErrors = bimap reverse reverse . foldl' accum ([], [])
  where
    accum (es, xs) (Left e)  = (e : es, xs)
    accum (es, xs) (Right x) = (es, x : xs)

-- | Runs a parser against all files in a directory (subdirectories included),
-- also allowing to keep an environment across all calls
parseDir :: Parser a
            -> FilePath
            -> (FilePath -> Either ParserError a -> StateT Env IO [b])
            -> StateT Env IO [b]
parseDir parser = runOnDir runP
  where
    runP path = lift (T.readFile path) >>= runParser parser path

