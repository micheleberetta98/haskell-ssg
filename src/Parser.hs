module Parser where

import           Data.Char
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Document
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

content :: Parser [Content]
content = many $ choice
  [ block
  , unquote
  , contentString
  ]

block :: Parser Content
block = parens "(" ")" (Block <$> identifier <*> option [] attrList <*> content)
  where
    attrList = parens "[" "]" $ many attrTuple
    attrTuple = (,) <$> identifier <*> stringedLiteral

unquote :: Parser Content
unquote = Unquote <$> (char '@' *> identifier)

contentString :: Parser Content
contentString = String <$> stringedLiteral

stringedLiteral :: Parser Text
stringedLiteral = between (char '"') (symbol "\"") $ T.pack <$> many
  ( choice
    [ char '\\' *> anySingle
    , anySingleBut '\"'
    ]
  )

identifier :: Parser Text
identifier = lexeme $ T.pack <$> some (noneOf ['\n', ' ', '(', ')', '[', ']'])

parens :: Text -> Text -> Parser a -> Parser a
parens open close = between (symbol open) (symbol close)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 empty empty
