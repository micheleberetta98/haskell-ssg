module Layout where

import           Document
import           Macro

applyLayout :: Macro -> Document -> [Content]
applyLayout macro@(Macro n _) (Document config content) = expand macro doc'
  where
    simpleList x = List x (AttrList [])
    doc' =
      [ simpleList n
        [ simpleList "pageTitle" [String (pageTitle config)]
        , simpleList "content" content
        ]
      ]
