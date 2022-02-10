module Macro.Internal where

import           Data.List
import           Data.Maybe
import           Data.Text  (Text)
import           Document

------------ Custom types

data Macro = Macro { name :: Text , body :: [Content] }
  deriving (Show, Eq)

type Params = [(Text, [Content])]

------------ Macro expansion

applyLayout :: [Macro] -> Document -> Maybe [Content]
applyLayout macros (Document config content) =
  expand <$> find (hasName layoutName) macros <*> pure content'
  where
    layoutName = configLayout config
    hasName x m = name m == x
    content' =
      [ List layoutName
        [ List "pageTitle" [String (pageTitle config)]
        , List "content" content
        ]
      ]

expandAll :: [Macro] -> [Content] -> [Content]
expandAll macros content = foldl' (flip expand) content macros

expand :: Macro -> [Content] -> [Content]
expand m@(Macro n body) = concatMap expand'
  where
    expand' el@(List h rest)
      | h == n    = expand m $ substitute (buildParams rest) body
      | otherwise = [List h (expand m rest)]
    expand' x     = [x]

------------ Substitution of @Unquote@s

substitute :: Params -> [Content] -> [Content]
substitute params = concat . mapMaybe (substitute' params)

substitute' :: Params -> Content -> Maybe [Content]
substitute' params (Unquote x)   = lookup x params
substitute' params (List h rest) = Just [List h (substitute params rest)]
substitute' _ x                  = Just [x]

------------ Utils

buildParams :: [Content] -> Params
buildParams = mapMaybe toParam
  where
    toParam (List h rest) = Just (h, rest)
    toParam _             = Nothing
