module Document where

import           Data.Bifunctor (Bifunctor (first))
import           Data.Text      (Text)
import qualified Data.Text      as T
import           ToHTML

------------ Custom types

-- | A 'Document' represents the entire document in the language,
-- comprised of a 'Config' and a list of 'Content
data Document = Document Config [Content]
  deriving (Show, Eq)

-- | The 'Config' of a 'Document' contains some metadata
-- about the document, like for example the page title or
-- the layout.
data Config = Config
  { pageTitle       :: Text
  , configCustomCss :: Maybe Text
  , configLayout    :: Text
  } deriving (Show, Eq)

-- | A 'Content' is essentialy a list or a single element ('Unquote' or 'String').
data Content
  = List Text AttrList [Content]
  | Unquote Text
  | String Text
  deriving (Show, Eq)

type AttrList = [(Text, Text)]

------------ Class instances

instance ToHTML Content where
  toHTML (String s)    = s
  toHTML (List p xs c) = tag p xs c
  toHTML _             = ""

toAttrList :: [(Text, Text)] -> Text
toAttrList = T.concat . map toPair
  where
    toPair (k, "") = k
    toPair (k, v)  = T.concat [k, "=\"", v, "\""]

tag :: Text -> AttrList -> [Content] -> Text
tag name xs content = T.concat
  [ "<", name, prependSpaceIfNotEmpty (toAttrList xs), ">"
  , foldMap toHTML content
  , "</", name, ">"
  ]
  where prependSpaceIfNotEmpty s = if T.null s then s else T.concat [" ", s]
