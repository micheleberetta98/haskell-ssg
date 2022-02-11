module Parser.Internal where

import           Control.Applicative.Permutations
import           Data.Char
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Void                        (Void)
import           Document
import           Macro
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L

------------ Custom types

type Parser = Parsec Void Text

type ParserError = ParseErrorBundle Text Void

------------ Main entities

document :: Parser Document
document = Document <$> config <*> many content

macro :: Parser Macro
macro = parens "(" ")"
  $ symbol "macro" *> (Macro <$> identifier <*> many content)

config :: Parser Config
config = parens "{" "}" $ runPermutation $
  Config
    <$> toPermutation (key "title")
    <*> toPermutationWithDefault Nothing (Just <$> key "custom-css")
    <*> toPermutationWithDefault "default" (key "layout")
  where
    key s = symbol s *> stringedLiteral

content :: Parser Content
content = recover $ choice
  [ unquote
  , contentString
  , list
  ]
  where
    recover = withRecovery $ \e -> do
      registerParseError e
      lexeme $ some (noneOf specialChars)
      pure (String "")

list :: Parser Content
list = parens "(" ")"
  $ identifier >>= chooseList
  where
    chooseList "alist" = AttrList <$> attrListContent
    chooseList x       = List x <$> many content

unquote :: Parser Content
unquote = Unquote <$> (char '@' *> identifier)

contentString :: Parser Content
contentString = String <$> stringedLiteral

------------ Utils

attrListContent :: Parser [(Text, Text)]
attrListContent = many (recover tuple)
  where
    tuple = parens "(" ")" $ (,) <$> identifier <*> option "" stringedLiteral
    recover = withRecovery $ \e -> do
      registerParseError e
      some (anySingleBut ')')
      symbol ")"
      pure ("", "")

stringedLiteral :: Parser Text
stringedLiteral = between (char '"') (symbol "\"") $
  T.pack <$> many
    ( choice
      [ char '\\' *> anySingle
      , anySingleBut '\"'
      ]
    )

identifier :: Parser Text
identifier = lexeme $ T.pack <$> some (noneOf specialChars)

parens :: Text -> Text -> Parser a -> Parser a
parens open close = between (symbol open) (symbol close)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 empty empty

specialChars :: [Char]
specialChars = [' ', '(', ')', '[', ']', '\"', '@', '\n']
