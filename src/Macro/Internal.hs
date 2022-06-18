{-# LANGUAGE TupleSections #-}

{-|
  Module      : Macro.Internal
  Description : The internals of 'Macro'

  This modules contains all the necessary data types and functions to work with
  the type 'Macro', their applications and expansion.
-}
module Macro.Internal where

import           Data.List
import           Data.Maybe
import           Data.Text  (Text)
import           Document

------------ Custom types

-- | A 'Macro' is defined by a name and some @body@
-- which is the stuff that will be substitued every time
-- the @name@ is encountered. Eventual 'Unquote' will be expanded
-- with the parameters. For example, @(macro foo (content bar))@ is
-- a macro named /foo/ that has a parameter /content/.
data Macro = Macro { macroName :: Text , macroBody :: [Content] }
  deriving (Show, Eq)

------------ Macro expansion

-- | Applies the first 'Macro' in @macros@ that has the same
-- name as the layout in the 'Document', with the default parameters
-- of /pageTitle/ and /content/.
applyLayout :: [Macro] -> Document -> Maybe [Content]
applyLayout macros (Document config content) =
  expand <$> find (hasName layoutName) macros <*> pure
      [ MacroCall layoutName
        [ MacroArg "pageTitle" [String (pageTitle config)]
        , MacroArg "customCss" [String (fromMaybe "" (configCustomCss config))]
        , MacroArg "content" content
        ]
      ]
  where
    layoutName = configLayout config
    hasName x m = macroName m == x

-- | Expands all macros in a list over a list of 'Document.Content'.
expandAll :: [Macro] -> [Content] -> [Content]
expandAll macros = fix (foldl' (flip expand))
  where
    fix f content
      | content' == content = content
      | otherwise             = fix f content'
      where
        content' = f content macros

-- | Expands a single 'Macro' over a list of 'Content'
expand :: Macro -> [Content] -> [Content]
expand m = concatMap (expandMacroCall m)

-- | Expands a single 'Macro' when encountering a 'MacroCall'
expandMacroCall :: Macro -> Content -> [Content]
expandMacroCall m@(Macro n body) (MacroCall name params)
  | n == name                            = expand m (substitute params body)
  | otherwise                            = [MacroCall name $ map (expandMacroArgBody m) params]
expandMacroCall m (List name attrs body) = [List name attrs (expand m body)]
expandMacroCall _ x                      = [x]

------------ Substitution

-- | Substitute all 'Unquote' with the corresponding parameter in a list of 'Content'.
substitute :: [MacroArg] -> [Content] -> [Content]
substitute params = concat . mapMaybe substitute'
  where
    substitute' (Unquote x)         = getMacroParam x params
    substitute' (List h attrs rest) = Just [List h (substituteAttrs params attrs) (substitute params rest)]
    substitute' x                   = Just [x]

-- | Substitute all 'AUnquote' with the corresponding parameter in an 'AttrList'.
substituteAttrs :: [MacroArg] -> AttrList -> AttrList
substituteAttrs params (AttrList as) = AttrList (mapMaybe substituteAttrs' as)
  where
    substituteAttrs' (k, AUnquote x) = (k,) <$> (getMacroParam x params >>= convert)
    substituteAttrs' (k, x)         = Just (k, x)

    convert [String s]  = Just (AString s)
    convert [Unquote x] = Just (AUnquote x)
    convert _           = Nothing

------------ Utils

-- | Little utility that expands a 'Macro' inside the body of a 'MacroArg'
expandMacroArgBody :: Macro -> MacroArg -> MacroArg
expandMacroArgBody m (MacroArg arg content) = MacroArg arg (expand m content)

-- | Extracts the content of a 'MacroArg' given a name
getMacroParam :: Text -> [MacroArg] -> Maybe [Content]
getMacroParam name params = getBody <$> find hasName params
  where
    getBody (MacroArg _ body) = body
    hasName(MacroArg n _) = n == name
