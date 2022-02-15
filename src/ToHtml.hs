module ToHtml where

import           Data.Text                       (Text, pack)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5                (Html)

class ToHtml a where
  toHtml :: a -> Html

instance ToHtml a => ToHtml [a] where
  toHtml = foldMap toHtml

render :: Html -> Text
render = pack . renderHtml
