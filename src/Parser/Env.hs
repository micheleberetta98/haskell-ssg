{-|
  Module      : Parser.Env
  Description : Environment for a parser

  Here it's specified the environment for a parser, consisting of valid
  list and attribute names.
-}
module Parser.Env where

import           Control.Monad.State
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import           Document.Internal   (defaultAttrNames, defaultListNames)

-- | The parser's environment.
data Env = Env
  { validListNames :: Set Text
  , validAttrNames :: Set Text
  , macroNames     :: Set Text
  } deriving (Show, Eq)

-- | The default 'Env', without any macro defined.
defaultEnv :: Env
defaultEnv = mkEnv defaultListNames defaultAttrNames []

-- | Smart constructor for 'Env'.
mkEnv :: [Text] -- ^ Valid list names
      -> [Text] -- ^ Valid attribute names
      -> [Text] -- ^ Macro names
      -> Env    -- ^ Resulting environment
mkEnv list attrs macros = Env (S.fromList list) (S.fromList attrs) (S.fromList macros)

-- | Adds a new macro name to the 'Env'.
addMacroName :: Text -> State Env ()
addMacroName name = modify' (\env -> env{ macroNames = S.insert name (macroNames env) })

-- | Checks if a name already exists anywhere in the 'Env'.
isNameTaken :: Text -> State Env Bool
isNameTaken name = do
  isMacro <- isValidMacroName name
  isList <- isValidListName name
  isAttr <- isValidAttrName name
  pure (isMacro || isList || isAttr)

-- | Returns @True@ if the name is in the valid macro names.
isValidMacroName :: Text -> State Env Bool
isValidMacroName = isValueIn macroNames

-- | Returns @True@ if the name is in the valid list names.
isValidListName :: Text -> State Env Bool
isValidListName = isValueIn validListNames

-- | Returns @True@ if the name is in the valid attribute names.
isValidAttrName :: Text -> State Env Bool
isValidAttrName = isValueIn validAttrNames

-- | Utility function to check if a name is in a specific list.
isValueIn :: (Env -> Set Text) -> Text -> State Env Bool
isValueIn f name = gets ((name `S.member`) . f)
