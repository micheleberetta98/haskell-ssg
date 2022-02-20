{-# LANGUAGE MultiWayIf #-}

{-|
  Module      : Parser.Internal
  Description : The internals of 'Parser'

  All the parsing rules for the language. Mainly, a 'document' and a 'macro' are considered the
  top level forms that a specific file can have.
  Here are also defined utilities, space consumers and special characters.
-}
module Parser.Internal where

import           Control.Applicative.Permutations
import           Control.Monad
import qualified Data.Set                         as S
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Document
import           Macro
import           Parser.Env
import           Prelude                          hiding (id)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L

------------ Custom types

-- | The custom parser.
type Parser = Parsec CustomError Text

-- | Parsing error (exported for type signatures).
type ParserError = ParseErrorBundle Text CustomError

------------ Custom errors

-- | A custom error type.
data CustomError
  = InvalidListName Text         -- ^ The name of an invalid 'Document.List'
  | InvalidAttrName Text         -- ^ The name of an invalid attribute
  | IdentifierAlreadyTaken Text  -- ^ The name of a macro that shadows an already existing name
  deriving (Eq, Show, Ord)

instance ShowErrorComponent CustomError where
  showErrorComponent = T.unpack . T.concat . errorMsg
    where
      errorMsg (InvalidListName name)        = ["\"", name, "\"", " is not a valid list name"]
      errorMsg (InvalidAttrName name)        = ["\"", name, "\"", " is not a valid attribute name"]
      errorMsg (IdentifierAlreadyTaken name) = ["\"", name, "\"", " is already declared and cannot be used as a macro"]

-- | Helper for a custom 'InvalidListName' error.
invalidListNameAt :: Int -> Text -> Parser ()
invalidListNameAt = registerCustomFailure InvalidListName

-- | Helper for a custom 'InvalidAttrName' error.
invalidAttrNameAt :: Int -> Text -> Parser ()
invalidAttrNameAt = registerCustomFailure InvalidAttrName

-- | Helper for a custom 'IdentifierAlreadyTaken' error.
idAlreadyTakenAt :: Int -> Text -> Parser ()
idAlreadyTakenAt = registerCustomFailure IdentifierAlreadyTaken

-- | Utility to register a 'CustomError'.
registerCustomFailure :: (Text -> CustomError) -> Int -> Text -> Parser ()
registerCustomFailure f o x = region (setErrorOffset o)
  $ registerFancyFailure $ S.singleton $ ErrorCustom $ f x

------------ Main entities

-- | Parses a 'Document', comprised of one 'Config' and zero or more 'content',
-- according to a valid list of names.
document :: Env -> Parser Document
document env = Document <$> config <*> many (content env)

-- | Parses a 'Macro' in the form @(macro \<id\> \<body\>)@,
-- where id is an 'identifier' and body is zero or more 'content'.
macro :: Env -> Parser Macro
macro env = do
  void (char '\'')
  parens "(" ")" $ do
    o <- getOffset
    id <- identifier <?> "macro name"
    body <- many (content env) <?> "macro body"
    if isNameTaken env id
      then idAlreadyTakenAt o id >> pure (Macro "" [])
      else pure (Macro id body)

-- | Parses a document 'Config' in the form @{ key \"value\" }@.
-- All key-value pairs are permutative and can appear in any order.
config :: Parser Config
config = parens "{" "}" $ runPermutation $
  Config
    <$> toPermutation (key "title")
    <*> toPermutationWithDefault Nothing (Just <$> key "custom-css")
    <*> toPermutationWithDefault "default" (key "layout")
  where
    key s = symbol s *> stringedLiteral <?> "a string literal"

-- | Parses a single 'Content', which can be a 'list', an 'unquote' or a 'contentString'.
-- This is a recoverable parser, and in case of an error it will consume all inputs
-- until one of 'specialChars' is encountered.
content :: Env -> Parser Content
content env = recover $ choice
  [ unquote
  , contentString
  , list env <?> "a list"
  ]
  where
    recover = withRecovery $ \e -> do
      registerParseError e
      void $ lexeme $ some (noneOf specialChars)
      pure (String "")

-- | Parses a single 'List'.
list :: Env -> Parser Content
list env = parens "(" ")" $ do
  o <- getOffset
  id <- identifier <?> "list or macro name"
  attrs <- option [] (attrList env) <?> "list attributes"

  if | isValidListName env id  -> List id attrs <$> many (content env) <?> "list body"
     | isValidMacroName env id -> List id attrs <$> many (list' env) <?> "macro parameters"
     | otherwise               -> do
       void $ many (content env) -- In order to report the errors
       invalidListNameAt o id >> pure (List "" [] [])

-- | Parses an 'Unquote', composed of @\@@ followed by an 'identifier'.
unquote :: Parser Content
unquote = Unquote <$> (char '@' *> identifier) <?> "an unquote (@)"

-- | Parses a 'Document.String' in the form of @\"anything goes\"@.
-- Newlines are permitted.
contentString :: Parser Content
contentString = String <$> stringedLiteral <?> "a string literal"

------------ Macro body

-- | A 'List' without the controls on the validity of the name.
-- Used for macro bodies, where list names are like argument names.
list' :: Env -> Parser Content
list' env = parens "(" ")" $ List <$> identifier <*> pure [] <*> many (content env)

------------ Utils

-- | An 'AttrList' is in the form of @[(param \"value") ...]@.
-- This is a recoverable parser, and in case of an error it will consume all inputs
-- until a single @)@ is encountered.
attrList :: Env -> Parser AttrList
attrList env = parens "[" "]" $ many $ recover (tuple <?> "a key-value tuple")
  where
    tuple = parens "(" ")" $ do
      o <- getOffset
      key <- identifier <?> "a key"
      value <- option (String "") (contentString <|> unquote) <?> "a string literal or an unquote"
      if isValidAttrName env key
        then pure (key, value)
        else region (setErrorOffset o) $ invalidAttrNameAt o key >> pure ("", String "")
    recover = withRecovery $ \e -> do
      registerParseError e
      void $ some (noneOf specialChars)
      void $ oneOf specialChars
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

-- | Wraps a parser in two opening and closing symbols.
parens :: Text -> Text -> Parser a -> Parser a
parens open close = between (symbol open) (symbol close)

-- | Adds a space consumer to a parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parses a specific string as a symbol.
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | A space consumer.
sc :: Parser ()
sc = L.space space1 empty empty

-- | Special chars, not permitted in identifiers.
specialChars :: [Char]
specialChars = [' ', '(', ')', '[', ']', '\"', '@', '\n']
