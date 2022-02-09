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

type AttrList = [(Text, Text)]
type Name = Text
