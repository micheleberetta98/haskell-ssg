{-|
  Module      : ToHtml
  Description : Conversion to HTML

  A utility module that defines a 'ToHtml' class and a 'render' function to obtain
  the pretty printed HTML.
-}
module ToHtml where

import           Data.Text                       (Text, pack)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Text.Blaze.Html5                (Html)

-- | The class of types that have an 'Html' representation.
class ToHtml a where
  -- | Transforms a generic type @a@ in 'Html'.
  toHtml :: a -> Html

instance ToHtml a => ToHtml [a] where
  toHtml = foldMap toHtml

-- | Renders the 'Html' as a strict 'Text'.
render :: Html -> Text
render = pack . renderHtml
