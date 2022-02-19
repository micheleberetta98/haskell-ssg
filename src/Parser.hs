{-|
  Module      : Parser
  Description : The parser for the language

  The main parsing rules for the language. A file can be either a 'document' or a 'macro'.
-}
module Parser
  ( Parser
  , ParserError
  , document
  , macro
  , mkEnv
  )
where

import           Parser.Env
import           Parser.Internal
