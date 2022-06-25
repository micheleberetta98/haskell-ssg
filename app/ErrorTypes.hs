module ErrorTypes where

import           Control.Exception
import           Parser

-- | Custom data types for parsing and building errors
data Errors
  = NoParse [ParserError]       -- ^ Parsing errors
  | BuildErrors [BuildError]    -- ^ Building errors
  deriving (Show)

instance Exception Errors

-- | Some errors can appear when building files, such as a layout or a 'Macro' missing
newtype BuildError = NoLayoutFound FilePath

instance Show BuildError where
  show (NoLayoutFound p) = concat ["(!) Something went wrong at ", p, ": maybe the layout doesn't exist?"]
