{-# LANGUAGE MultiWayIf #-}

{-|
  Module      : Parser.Internal
  Description : The internals of 'Parser'

  All the parsing rules for the language. Mainly, a 'Document.Document' and a 'macro' are considered the
  top level forms that a specific file can have.
  Here are also defined utilities, space consumers and special characters.
-}
module Parser.Internal where

import           Control.Applicative.Permutations
import           Control.Monad
import           Control.Monad.State
import qualified Data.Set                         as S
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Document
import           Macro
import           Parser.Env
import           Prelude                          hiding (id)
import           Text.Megaparsec                  hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer       as L

------------------------------------------------------------------------------------
-- * Types

-- | The custom parser.
type Parser = ParsecT CustomError Text (State Env)

-- | Parsing error (exported for type signatures).
type ParserError = ParseErrorBundle Text CustomError

-- | A custom error type.
data CustomError
  = InvalidListName Text         -- ^ The name of an invalid 'Document.List'.
  | InvalidAttrName Text         -- ^ The name of an invalid attribute key.
  | IdentifierAlreadyTaken Text  -- ^ The name of a macro that shadows an already existing name.
  deriving (Eq, Show, Ord)

instance ShowErrorComponent CustomError where
  showErrorComponent = T.unpack . T.concat . errorMsg
    where
      errorMsg (InvalidListName name)        = ["\"", name, "\"", " is not a valid list name"]
      errorMsg (InvalidAttrName name)        = ["\"", name, "\"", " is not a valid attribute name"]
      errorMsg (IdentifierAlreadyTaken name) = ["\"", name, "\"", " is already declared and cannot be used as a macro"]

------------------------------------------------------------------------------------
-- * Error helpers

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
registerCustomFailure f o = region (setErrorOffset o) . registerFancyFailure . S.singleton . ErrorCustom . f

------------------------------------------------------------------------------------
-- * Main entities

-- | Parses a 'Document.Document', comprised of one 'Document.Config' and zero or more 'Document.Content',
-- according to a valid list of names.
document :: Parser Document
document = Document <$> config <*> many content

-- | Parses a 'Macro' in the form @'(macro \<id\> \<body\>)@,
-- where id is an 'identifier' and body is zero or more 'Document.Content'.
macro :: Parser Macro
macro = parens "'(" ")" $ do
  o <- getOffset
  id <- identifier <?> "macro name"
  body <- many content <?> "macro body"
  nameTaken <- lift (isNameTaken id)
  if nameTaken
    then idAlreadyTakenAt o id >> pure (Macro "" [])
    else lift (addMacroName id) >> pure (Macro id body)

-- | Little utility for parsing multime macros.
macros :: Parser [Macro]
macros = many macro

-- | Parses a document 'Document.Config' in the form @{ key \"value\" }@.
-- All key-value pairs are permutative and can appear in any order.
config :: Parser Config
config = parens "{" "}" $ runPermutation $
  Config
    <$> toPermutation (key "title")
    <*> toPermutationWithDefault Nothing (Just <$> key "custom-css")
    <*> toPermutationWithDefault "default" (key "layout")
  where
    key s = symbol s *> stringedLiteral <?> "a string literal"

-- | Parses a single 'Document.Content', which can be a 'listOrMacro', an 'unquote' or a 'contentString'.
-- This is a recoverable parser, and in case of an error it will consume all inputs
-- until one of 'specialChars' is encountered.
content :: Parser Content
content = recover $ choice
  [ unquote         <?> "an unquote (@)"
  , contentString   <?> "a string"
  , listOrMacro     <?> "a list or a macro call"
  ]
  where
    recover = withRecovery $ \e -> do
      registerParseError e
      void $ lexeme $ some (noneOf specialChars)
      pure (String "")

-- | Parses a single list, which can be a 'MacroCall' or a 'List'.
listOrMacro :: Parser Content
listOrMacro = parens "(" ")" $ do
    o <- getOffset
    id <- identifier <?> "list or macro name"
    isMacro <- lift (isValidMacroName id)
    if isMacro
      then MacroCall id <$> many macroArg <?> "macro parameters"
      else do
        attrs <- option (AttrList []) attrList <?> "attribute list"
        body <- many content <?> "list body"
        isList <- lift (isValidListName id)
        if isList
          then pure (List id attrs body)
          else invalidListNameAt o id >> pure (List "" (AttrList []) [])

-- | Parses an 'Unquote', composed of @\@@ followed by an 'identifier'.
unquote :: Parser Content
unquote = Unquote <$> (unquoteToken *> identifier) <?> "an unquote (@)"

-- | Parses a 'Document.String' in the form of @\"anything goes\"@.
-- Newlines are permitted.
contentString :: Parser Content
contentString = String <$> stringedLiteral <?> "a string literal"

------------------------------------------------------------------------------------
-- * Macro arguments

-- | A 'MacroArg' parser, i.e. a list without the controls on the validity of the name.
-- Used for macro bodies, where list names are like argument names.
macroArg :: Parser MacroArg
macroArg = parens "(" ")" $
  MacroArg
  <$> (identifier <?> "parameter's name")
  <*> many content

------------------------------------------------------------------------------------
-- * Attribute lists

-- | An 'AttrList' is in the form of @[(param \"value") ...]@.
-- This is a recoverable parser, and in case of an error it will consume all inputs
-- until a single @)@ is encountered.
attrList :: Parser AttrList
attrList = AttrList <$> (parens "[" "]" $ many $ recover (tuple <?> "a key-value tuple"))
  where
    tuple = parens "(" ")" $ do
      o <- getOffset
      key <- identifier <?> "a key"
      value <- option (AString "") (attrPairString <|> attrPairUnquote) <?> "a string literal or an unquote"
      isAttrName <- lift (isValidAttrName key)
      if isAttrName
        then pure (key, value)
        else region (setErrorOffset o) $ invalidAttrNameAt o key >> pure ("", AString "")
    recover = withRecovery $ \e -> do
      registerParseError e
      void $ some (noneOf specialChars)
      void $ oneOf specialChars
      pure ("", AString "")

-- | A parser for a string literal in the context of attribute lists.
attrPairString :: Parser AttrPairValue
attrPairString = AString <$> stringedLiteral <?> "a string literal"

-- | A parser for an unquote in the context of attribute lists.
attrPairUnquote :: Parser AttrPairValue
attrPairUnquote = AUnquote <$> (unquoteToken *> identifier) <?> "an unquote (@)"

------------------------------------------------------------------------------------
-- * Utils

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

-- | The token that marks the beginning of an unquote
unquoteToken :: Parser Char
unquoteToken = char '@'

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
