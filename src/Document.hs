module Document where

import           Data.Text (Text)

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

type Layout = [Content]

------------ Layout expansion

applyLayout :: Document -> [(Name, Layout)] -> Maybe [Content]
applyLayout doc@(Document config content) layouts = expand doc <$> lookup (layout config) layouts

expand :: Document -> Layout -> [Content]
expand (Document config content) = concatMap expand'
  where
    expand' (Unquote "pageTitle") = [String (pageTitle config)]
    expand' (Unquote "content")   = content
    expand' (Block b attrList c)  = [Block b attrList (concatMap expand' c)]
    expand' (String s)            = [String s]
    expand' _                     = []
