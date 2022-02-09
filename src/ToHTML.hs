module ToHTML where

import           Data.Text (Text)

class ToHTML a where
  toHTML :: a -> Text
