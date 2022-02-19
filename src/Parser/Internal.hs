{-|
  Module      : Parser.Internal
  Description : The internals of 'Parser'

  All the parsing rules for the language. Mainly, a 'document' and a 'macro' are considered the
  top level forms that a specific file can have.
  Here are also defined utilities, space consumers and special characters.
-}
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

-- | The custom parser
type Parser = Parsec CustomError Text

-- | Parsing error (exported for type signatures)
type ParserError = ParseErrorBundle Text CustomError

------------ Custom errors

-- | A custom error type
data CustomError
  = InvalidListName Text  -- ^ The name of an invalid 'Document.List'
  | InvalidAttrName Text  -- ^ The name of an invalid attribute
  deriving (Eq, Show, Ord)

instance ShowErrorComponent CustomError where
  showErrorComponent (InvalidListName name) = T.unpack name ++ " is not a valid block name"
  showErrorComponent (InvalidAttrName name)  = T.unpack name ++ " is not a valid attribute name"

-- | Helper for a custom 'InvalidListName' error
invalidListName :: Text -> Parser a
invalidListName = customFailure . InvalidListName

-- | Helper for a custom 'InvalidAttrName' error
invalidAttrName :: Text -> Parser a
invalidAttrName = customFailure . InvalidAttrName

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

-- | Parses a single 'List'.
list :: Parser Content
list = parens "(" ")" $
  List <$> identifier <*> option [] attrList <*> many content

-- | Parses an 'Unquote', composed of @\@@ followed by an 'identifier'.
unquote :: Parser Content
unquote = Unquote <$> (char '@' *> identifier)

-- | Parses a 'Document.String' in the form of @\"anything goes\"@.
-- Newlines are permitted.
contentString :: Parser Content
contentString = String <$> stringedLiteral

------------ Utils

-- | An 'AttrList' is in the form of @[(param \"value") ...]@.
-- This is a recoverable parser, and in case of an error it will consume all inputs
-- until a single @)@ is encountered.
attrList :: Parser AttrList
attrList = parens "[" "]" $ many $ recover
  $ parens "(" ")"
  $ (,) <$> identifier <*> option (String "") (contentString <|> unquote)
  where
    recover = withRecovery $ \e -> do
      registerParseError e
      some (noneOf specialChars)
      oneOf specialChars
      pure ("", String "")

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

-- | Wraps a parser in two opening and closing symbols
parens :: Text -> Text -> Parser a -> Parser a
parens open close = between (symbol open) (symbol close)

-- | Adds a space consumer to a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parses a specific string as a symbol
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | A space consumer
sc :: Parser ()
sc = L.space space1 empty empty

-- | Special chars, not permitted in identifiers.
specialChars :: [Char]
specialChars = [' ', '(', ')', '[', ']', '\"', '@', '\n']
