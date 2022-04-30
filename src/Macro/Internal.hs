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

-- | A little utility type to represent macro params.
type MacroParams = [(Text, [Content])]

------------ Macro expansion

-- | Applies the first 'Macro' in @macros@ that has the same
-- name as the layout in the 'Document', with the default parameters
-- of /pageTitle/ and /content/.
applyLayout :: [Macro] -> Document -> Maybe [Content]
applyLayout macros (Document config content) =
  expand <$> find (hasName layoutName) macros <*> pure
      [ List layoutName []
        [ List "pageTitle" [] [String (pageTitle config)]
        , List "customCss" [] [String (fromMaybe "" (configCustomCss config))]
        , List "content" [] content
        ]
      ]
  where
    layoutName = configLayout config
    hasName x m = macroName m == x

-- | Expands all macros in a list over a list of 'Document.Content'.
expandAll :: [Macro] -> [Content] -> [Content]
expandAll macros content = foldl' (flip expand) content macros

-- | Expands a single macro over a list of 'Document.Content'.
expand :: Macro -> [Content] -> [Content]
expand m@(Macro n body) = concatMap expand'
  where
    expand' (List h attrs rest)
      | h == n    = expand m (substitute params body)
      | otherwise = [List h (substituteAttrs params attrs) (expand m rest)]
      where params = buildParams rest
    expand' x     = [x]

------------ Substitution

-- | Substitute all 'Unquote' with the corresponding parameter in a list of 'Content'.
substitute :: MacroParams -> [Content] -> [Content]
substitute params = concat . mapMaybe substitute'
  where
    substitute' (Unquote x)         = lookup x params
    substitute' (List h attrs rest) = Just [List h (substituteAttrs params attrs) (substitute params rest)]
    substitute' x                   = Just [x]

-- | Substitute all 'Unquote' with the corresponding parameter in an 'AttrList'.
substituteAttrs :: MacroParams -> AttrList -> AttrList
substituteAttrs params = mapMaybe substituteAttrs'
  where
    substituteAttrs' (k, Unquote x) = (k,) <$> (lookup x params >>= listToMaybe)
    substituteAttrs' (k, x)         = Just (k, x)

------------ Utils

-- | Builds the 'MacroParams' data structure from the body.
-- of a macro
buildParams :: [Content] -> MacroParams
buildParams = mapMaybe $ \case
    (List h _ rest) -> Just (h, rest)
    _               -> Nothing
