{-# LANGUAGE LambdaCase #-}

module Macro.Internal where

import           Data.List
import           Data.Maybe
import           Data.Text         (Text)
import           Document.Internal

------------ Custom types

-- | A 'Macro' is defined by a name and some @body@
-- which is the stuff that will be substitued every time
-- the @name@ is encountered. Eventual 'Unquote' will be expanded
-- with the parameters. For example, @(macro foo (content bar))@ is
-- a macro named /foo/ that has a parameter /content/.
data Macro = Macro { name :: Text , body :: [Content] }
  deriving (Show, Eq)

-- | A little utility type to represent macro params
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
      , List "content" [] content
      ]
    ]
  where
    layoutName = configLayout config
    hasName x m = name m == x

-- | Expands all macros in a list over a list of 'Content'.
expandAll :: [Macro] -> [Content] -> [Content]
expandAll macros content = foldl' (flip expand) content macros

-- | Expands a single macro over a list of 'Content'
expand :: Macro -> [Content] -> [Content]
expand m@(Macro n body) = concatMap expand'
  where
    expand' el@(List h attrs rest)
      | h == n    = expand m $ substitute (buildParams rest) body
      | otherwise = [List h attrs (expand m rest)]
    expand' x     = [x]

------------ Substitution

-- | Substitute all 'Unquote' with the corresponding parameter
substitute :: MacroParams -> [Content] -> [Content]
substitute params = concat . mapMaybe (substitute' params)
  where
    substitute' params (Unquote x)         = lookup x params
    substitute' params (List h attrs rest) = Just [List h attrs (substitute params rest)]
    substitute' _ x                        = Just [x]

------------ Utils

-- | Builds the 'MacroParams' data structure from the body
-- of a macro
buildParams :: [Content] -> MacroParams
buildParams = mapMaybe $ \case
    (List h _ rest) -> Just (h, rest)
    _               -> Nothing
