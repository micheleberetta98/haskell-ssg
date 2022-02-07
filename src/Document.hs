module Document where

import           Data.Text (Text)

data Document = Document Config [Content]
  deriving (Show)

data Config = Config
  { pageTitle :: Text
  , customCss :: Maybe Text
  , layout    :: Name
  } deriving (Show)

data Content
  = Block Name AttrList [Content]
  | Unquote Name
  | String Text
  deriving (Show)

type AttrList = [(Text, Text)]
type Name = Text
