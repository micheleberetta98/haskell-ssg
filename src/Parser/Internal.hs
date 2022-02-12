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

-- | Parses a 'Document', comprised of one 'Config' and zero or more 'content'.
document :: Parser Document
document = Document <$> config <*> many content

-- | Parses a 'Macro' in the form @(macro <id> <body>)@,
-- where id is an 'identifier' and body is zero or more 'content'.
macro :: Parser Macro
macro = parens "(" ")"
  $ symbol "macro" *> (Macro <$> identifier <*> many content)

-- | Parses a document 'Config' in the form @{ key \"value\" }@.
-- All key-value pairs are permutative and can appear in any order.
config :: Parser Config
config = parens "{" "}" $ runPermutation $
  Config
    <$> toPermutation (key "title")
    <*> toPermutationWithDefault Nothing (Just <$> key "custom-css")
    <*> toPermutationWithDefault "default" (key "layout")
  where
    key s = symbol s *> stringedLiteral

-- | Parses a single 'Content', which can be a 'list', an 'unquote' or a 'contentString'.
-- This is a recoverable parser, and in case of an error it will consume all inputs
-- until one of 'specialChars' is encountered.
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

-- | Parses a single 'List' or 'AttrList'.
list :: Parser Content
list = parens "(" ")"
  $ identifier >>= chooseList
  where
    chooseList "alist" = AttrList <$> attrListContent
    chooseList x       = List x <$> many content

-- | Parses an 'Unquote', composed of @\@@ followed by an 'identifier'.
unquote :: Parser Content
unquote = Unquote <$> (char '@' *> identifier)

-- | Parses a 'String' in the form of @\"anything goes\"@.
-- Newlines are permitted.
contentString :: Parser Content
contentString = String <$> stringedLiteral

------------ Utils

-- | An 'AttrList' content in the form of @((param \"value") ...)@.
-- This is a recoverable parser, and in case of an error it will consume all inputs
-- until a single @)@ is encountered.
attrListContent :: Parser [(Text, Text)]
attrListContent = many (recover tuple)
  where
    tuple = parens "(" ")" $ (,) <$> identifier <*> option "" stringedLiteral
    recover = withRecovery $ \e -> do
      registerParseError e
      some (anySingleBut ')')
      symbol ")"
      pure ("", "")

-- | A 'stringedLiteral' is some 'Text' between two @\"@ characters.
-- It can be empty.
stringedLiteral :: Parser Text
stringedLiteral = between (char '"') (symbol "\"") $
  T.pack <$> many
    ( choice
      [ char '\\' *> anySingle
      , anySingleBut '\"'
      ]
    )

-- | An 'identifier' is some 'Text' with no 'specialChars' and cannot be empty.
identifier :: Parser Text
identifier = lexeme $ T.pack <$> some (noneOf specialChars)

parens :: Text -> Text -> Parser a -> Parser a
parens open close = between (symbol open) (symbol close)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | A space consumer
sc :: Parser ()
sc = L.space space1 empty empty

-- | Special chars, not permitted in identifiers.
specialChars :: [Char]
specialChars = [' ', '(', ')', '[', ']', '\"', '@', '\n']
