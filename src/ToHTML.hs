module ToHTML where

import           Data.Text (Text)
import qualified Data.Text as T

class ToHTML a where
  toHTML :: a -> Text

instance ToHTML a => ToHTML [a] where
  toHTML = T.concat . map toHTML
