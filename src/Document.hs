module Document where

import           Data.Text (Text)
import qualified Data.Text as T
import           ToHTML

------------ Custom types

data Document = Document Config [Content]
  deriving (Show, Eq)

data Config = Config
  { pageTitle       :: Text
  , configCustomCss :: Maybe Text
  , configLayout    :: Text
  } deriving (Show, Eq)

data Content
  = List Text AttrList [Content]
  | Quote Content
  | Unquote Text
  | String Text
  deriving (Show, Eq)

newtype AttrList = AttrList [(Text, Text)]
  deriving (Show, Eq)

------------ Class instances

instance ToHTML a => ToHTML [a] where
  toHTML = T.concat . map toHTML

instance ToHTML Content where
  toHTML (String s)   = s
  toHTML (List p a c) = tag' p a (toHTML c)
  toHTML _            = ""

instance ToHTML AttrList where
  toHTML (AttrList attrs) = T.concat (map toPair attrs)
    where toPair (n, v) = T.concat [n, "=\"", v, "\""]

tag :: Text -> Text -> Text
tag name = tag' name (AttrList [])

tag' :: Text -> AttrList -> Text -> Text
tag' name attrList content = T.concat
  [ "<", name, prependSpaceIfNotEmpty (toHTML attrList), ">"
  , content
  , "</", name, ">"
  ]
  where prependSpaceIfNotEmpty s = if T.null s then s else T.concat [" ", s]
