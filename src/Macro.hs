{-|
  Module      : Macro
  Description : Macro definition and expansions

  The 'Macro' module exports the 'Macro' type definition and the necessary functions
  to expand it on a list of 'Content'. The function 'applyLayout' works on 'Document'
  and applies the macro that has the same name as the layout property on the 'Config' part
  of the 'Document'.
-}
module Macro
  ( expand
  , expandAll
  , applyLayout
  , Macro(..)
  , Layout
  )
where

import           Macro.Internal

-- | A useful alias
type Layout = Macro
