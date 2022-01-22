module Document where

import           Data.Text (Text)

data Document = Document Config [Content]
  deriving (Show)

data Config = Config
  { pageTitle :: Text
  , customCss :: Maybe Text
  , layout    :: Text
  } deriving (Show)

data Content
  = Block Text AttrList [Content]
  | Unquote Text
  | Literals Text
  deriving (Show)

type AttrList = [(Text, Text)]
