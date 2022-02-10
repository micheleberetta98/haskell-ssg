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

expandAll :: [Macro] -> [Content] -> [Content]
expandAll macros content = foldl' (flip expand) content macros

expand :: Macro -> [Content] -> [Content]
expand m@(Macro n body) = concatMap expand'
  where
    expand' el@(List h attrs rest)
      | h == n    = expand m $ substitute (buildParams rest) body
      | otherwise = [List h attrs (expand m rest)]
    expand' x     = [x]

------------ Substitution of @Unquote@s

substitute :: Params -> [Content] -> [Content]
substitute params = concat . mapMaybe (substitute' params)

substitute' :: Params -> Content -> Maybe [Content]
substitute' params (Unquote x)         = lookup x params
substitute' params (List h attrs rest) = Just [List h attrs (substitute params rest)]
substitute' _ x                        = Just [x]

------------ Utils

buildParams :: [Content] -> Params
buildParams = mapMaybe toParam
  where
    toParam (List h _ rest) = Just (h, rest)
    toParam _               = Nothing
