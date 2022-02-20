{-|
  Module      : Parser.Env
  Description : Environment for a 'Parser'

  Here it's specified the environment for a Parser, consisting of valid
  list and attribute names.
-}
module Parser.Env where

import           Data.Set          (Set)
import qualified Data.Set          as S
import           Data.Text         (Text)
import           Document.Internal (defaultAttrNames, defaultListNames)

-- | The 'Parser' environment.
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
addMacroName :: Text -> Env -> Env
addMacroName name env = env{ macroNames = S.insert name (macroNames env) }

-- | Checks if a name already exists anywhere in the 'Env'.
isNameTaken :: Env -> Text -> Bool
isNameTaken env name = any (\f -> f env name) [isValidMacroName, isValidListName, isValidAttrName]

-- | Returns @True@ if the name is in the valid macro names.
isValidMacroName :: Env -> Text -> Bool
isValidMacroName = isValueIn macroNames

-- | Returns @True@ if the name is in the valid list names.
isValidListName :: Env -> Text -> Bool
isValidListName = isValueIn validListNames

-- | Returns @True@ if the name is in the valid attribute names.
isValidAttrName :: Env -> Text -> Bool
isValidAttrName = isValueIn validAttrNames

-- | Utility function to check if a name is in a specific list.
isValueIn :: (Env -> Set Text) -> Env -> Text -> Bool
isValueIn f env name = name `S.member` f env
