{-|
  Module      : Document
  Description : Representations of documents

  The 'Document' module offers all type definitions for representing a 'Document', which is
  comprised of a 'Config' and some 'Content'.
  The 'Content' itself can be a 'List', an 'Unquote' or a 'String'. If it is a 'List', then it
  can contain other 'Content', as well as an 'AttrList', which is a list of tuples representing
  all the possible attributes that the corresponding HTML tag can have.
-}
module Document
  ( Document(..)
  , Config(..)
  , Content(..)
  , AttrList
  , MacroArg(..)
  )
where

import           Document.Internal
