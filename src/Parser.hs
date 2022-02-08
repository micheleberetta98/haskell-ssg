module Parser where

import           Control.Applicative.Permutations
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Void                        (Void)
import           Document
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L

type Parser = Parsec Void Text

config :: Parser Config
config = parens "{" "}" $ runPermutation $
  Config
    <$> toPermutation (key "title")
    <*> toPermutationWithDefault Nothing (Just <$> key "custom-css")
    <*> toPermutationWithDefault "default" (key "layout")
  where
    key s = symbol s *> stringedLiteral

content :: Parser Content
content = choice
  [ block
  , unquote
  , contentString
  ]

block :: Parser Content
block = parens "(" ")"  $
  Block
    <$> identifier
    <*> option [] attrList
    <*> (blockContent <|> pure [])
  where
    attrList = parens "[" "]" (many attrTuple)
    attrTuple = (,) <$> identifier <*> stringedLiteral
    blockContent = ([] <$ eof) <|> do
      c <- lookAhead anySingle
      if c == ')'
        then pure []
        else (:) <$> recover content <*> blockContent
    recover = withRecovery $ \e -> do
      registerParseError e
      many (anySingleBut ' ')
      char ' '
      pure (String "")

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
