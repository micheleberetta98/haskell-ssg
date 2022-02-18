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
  )
where

import           Parser.Internal
