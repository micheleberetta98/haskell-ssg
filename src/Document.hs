module Document where

import           Data.Bifunctor (Bifunctor (first))
import           Data.List
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
-- A list can be "general" or an 'AttrList', which has the form of
-- @(attrlist (param1 "value1") (param2 "value2") (paramWithNoValue))@
data Content
  = List Text [Content]
  | AttrList [(Text, Text)]
  | Unquote Text
  | String Text
  deriving (Show, Eq)

------------ Functions

-- | Merges multiple 'AttrList' into one at the begininng of the list of 'Content'.
mergeAttrLists :: [Content] -> [Content]
mergeAttrLists = tcons . first AttrList . foldl' merge ([], [])
  where
    merge (attrs, xs) (AttrList as) = (attrs ++ as, xs)
    merge (attrs, xs) x             = (attrs, xs ++ [x])

    tcons (x, xs) = x : xs

------------ Class instances

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
