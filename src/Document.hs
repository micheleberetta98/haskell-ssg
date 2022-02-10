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
  = List Text [Content]
  | AttrList [(Text, Text)]
  | Unquote Text
  | String Text
  deriving (Show, Eq)

------------ Class instances

instance ToHTML a => ToHTML [a] where
  toHTML = T.concat . map toHTML

instance ToHTML Content where
  toHTML (String s)    = s
  toHTML (List p c)    = tag p c
  toHTML (AttrList xs) = toAttrList xs
  toHTML _             = ""

toAttrList :: [(Text, Text)] -> Text
toAttrList = T.concat . map toPair
  where
    toPair (k, "") = k
    toPair (k, v)  = T.concat [k, "=\"", v, "\""]

tag :: Text -> [Content] -> Text
tag name (AttrList xs : content) = T.concat
  [ "<", name, prependSpaceIfNotEmpty (toAttrList xs), ">"
  , toHTML content
  , "</", name, ">"
  ]
  where prependSpaceIfNotEmpty s = if T.null s then s else T.concat [" ", s]
tag name content = tag name (AttrList [] : content)
