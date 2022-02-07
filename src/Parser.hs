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

stringedLiteral :: Parser Text
stringedLiteral = between (char '"') (symbol "\"") $ T.pack <$> many
  ( choice
    [ char '\\' *> anySingle
    , anySingleBut '\"'
    ]
  )

identifier :: Parser Text
identifier = lexeme $ T.pack <$> do
  c  <- satisfy isAlpha
  cs <- many (noneOf ['\n', ' ', '(', ')', '[', ']'])
  pure (c : cs)

parens :: Text -> Text -> Parser a -> Parser a
parens open close = between (symbol open) (symbol close)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 empty empty
