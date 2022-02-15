module ToHTML where

import           Text.Blaze.Html5 (Html)

class ToHtml a where
  toHtml :: a -> Html

-- instance ToHTML a => ToHTML [a] where
--   toHTML = T.concat . map toHTML
