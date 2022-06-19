{-|
  Module      : Document
  Description : Representations of documents

  The 'Document.Document' module offers all type definitions for representing a 'Document.Document', which is
  comprised of a 'Document.Config' and some 'Document.Content'.
  The 'Document.Content' itself can be a 'List', an 'Unquote' or a 'Document.Content.String'. If it is a 'List', then it
  can contain other 'Document.Content', as well as an 'AttrList', which is a list of tuples representing
  all the possible attributes that the corresponding HTML tag can have.
-}
module Document
  ( -- * Types
    Document(..)
  , Config(..)
  , Content(..)
  , AttrList(..)
  , AttrPairValue(..)
  , MacroArg(..)
  )
where

import           Document.Internal
