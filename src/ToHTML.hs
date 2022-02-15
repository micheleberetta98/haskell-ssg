module ToHtml where

-- import           Text.Blaze.Html5 (Html)
import           Data.Text (Text)

class ToHtml a where
  toHtml :: a -> Text

-- instance ToHTML a => ToHTML [a] where
--   toHTML = T.concat . map toHTML
