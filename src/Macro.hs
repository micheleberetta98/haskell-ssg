module Macro where

import           Data.Text (Text)
import           Document

data Macro = Macro
  { name :: Text
  , body :: [Content]
  }
  deriving (Show, Eq)
