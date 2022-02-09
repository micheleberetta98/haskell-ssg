module Document where

import           Data.Text (Text)
import qualified Data.Text as T
import           ToHTML

------------ Custom types

data Document = Document Config [Content]
  deriving (Show, Eq)

data Config = Config
  { pageTitle :: Text
  , customCss :: Maybe Text
  , layout    :: Name
  } deriving (Show, Eq)

data Content
  = Block Name AttrList [Content]
  | Unquote Name
  | String Text
  deriving (Show, Eq)

newtype AttrList = AttrList [(Text, Text)]
  deriving (Show, Eq)

type Name = Text

newtype Layout = Layout [Content]
  deriving (Show, Eq)

------------ Layout expansion

applyLayout :: Document -> [(Name, Layout)] -> Maybe [Content]
applyLayout doc@(Document config content) layouts = expand doc <$> lookup (layout config) layouts

expand :: Document -> Layout -> [Content]
expand (Document config content) (Layout l) = concatMap expand' l
  where
    expand' (Unquote "pageTitle") = [String (pageTitle config)]
    expand' (Unquote "content")   = content
    expand' (Block b attrList c)  = [Block b attrList (concatMap expand' c)]
    expand' (String s)            = [String s]
    expand' _                     = []

------------ Class instances

instance ToHTML a => ToHTML [a] where
  toHTML = T.concat . map toHTML

instance ToHTML Content where
  toHTML (String s)    = s
  toHTML (Block p a c) = tag' p a (toHTML c)
  toHTML _             = ""

instance ToHTML AttrList where
  toHTML (AttrList attrs) = T.concat (map toPair attrs)
    where toPair (n, v) = T.concat [n, "=\"", v, "\""]

tag :: Name -> Text -> Text
tag name = tag' name (AttrList [])

tag' :: Name -> AttrList -> Text -> Text
tag' name attrList content = T.concat
  [ "<", name, prependSpaceIfNotEmpty (toHTML attrList), ">"
  , content
  , "</", name, ">"
  ]
  where prependSpaceIfNotEmpty s = if T.null s then s else T.concat [" ", s]
