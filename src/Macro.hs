{-|
  Module      : Macro
  Description : Macro definition and expansions

  The 'Macro' module exports the 'Macro' type definition and the necessary functions
  to expand it on a list of 'Document.Content'. The function 'applyLayout' works on 'Document.Document'
  and applies the macro that has the same name as the layout property on the 'Document.Config' part
  of the 'Document.Document'.
-}
module Macro
  ( -- * Types
    Macro(..)
  , Layout
  -- * Macro expansion
  , expand
  , expandAll
  , applyLayout
  )
where

import           Macro.Internal
