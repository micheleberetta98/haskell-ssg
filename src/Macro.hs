module Macro where

import           Data.List
import           Data.Maybe
import           Data.Text  (Text)
import           Document

data Macro = Macro
  { name :: Text
  , body :: [Content]
  }
  deriving (Show, Eq)

expand :: Macro -> [Content] -> [Content]
expand m@(Macro n body) = concatMap expand'
  where
    expand' el@(List h attrs rest)
      | n `headOf` el = expand m (substitute rest body)
      | otherwise     = [List h attrs (expand m rest)]
    expand' x         = [x]

substitute :: [Content] -> [Content] -> [Content]
substitute params = concat . mapMaybe (substitute' params)

substitute' :: [Content] -> Content -> Maybe [Content]
substitute' params (Unquote x)         = find (x `headOf`) params >>= getContent
  where
    getContent (List _ _ x) = Just x
    getContent _            = Nothing
substitute' params (List h attrs rest) = Just [List h attrs (substitute params rest)]
substitute' _ x                        = Just [x]

headOf :: Text -> Content -> Bool
x `headOf` (List h _ _) = x == h
_ `headOf` _            = False
