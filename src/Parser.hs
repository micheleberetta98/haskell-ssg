{-|
  Module      : Parser
  Description : The parser for the language

  The main parsing rules for the language. A file can be either a 'Document.Document' or a 'macro'.
-}
module Parser
  ( -- * Types
    Parser
  , ParserError
  , Env
  -- * Main parsers
  , document
  , macro
  , macros
  -- * Environment
  , mkEnv
  , addMacroName
  , defaultEnv
  -- * Running a parser
  , runParser
  , parseWithEnv
  -- * Pretty printing errors
  , prettyParserError
  )
where

import           Control.Monad.State
import           Data.Text           (Text)
import           Parser.Env
import           Parser.Internal
import qualified Text.Megaparsec     as M

-- | Runs a parser @p@ on a content of some file, inside a @StateT Env m@
runParser :: Monad m => Parser a -> FilePath -> Text -> StateT Env m (Either ParserError a)
runParser p path text = state $ runState $ M.runParserT (p <* M.eof) path text

-- | Runs a parser @p@ on a content of some file and with a custom environment
parseWithEnv :: Env -> Parser a -> FilePath -> Text -> Either ParserError a
parseWithEnv env p path text = evalState (M.runParserT p path text) env

-- | Re-export of 'Text.Megaparsec.errorBundlePretty'
prettyParserError :: (M.VisualStream a, M.TraversableStream a, M.ShowErrorComponent b) => M.ParseErrorBundle a b -> String
prettyParserError = M.errorBundlePretty
