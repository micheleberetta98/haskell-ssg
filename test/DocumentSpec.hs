module DocumentSpec (documentSpec) where

import           Document
import           Macro.Internal
import           Test.Hspec
import           Text.Megaparsec (many)

applyAll :: [Macro] -> [Macro] -> Document -> Maybe [Content]
applyAll macros layouts = fmap (expandAll macros) . applyLayout layouts

layouts :: [Macro]
layouts =
  [ Macro "default"
    [ List "html" (AttrList [])
      [ List "head" (AttrList [])
        [ List "pagetitle" (AttrList []) [Unquote "pageTitle"]
        , List "link_" (AttrList [("rel", AString "stylesheet"), ("href", AUnquote "customCss")]) []
        ]
      , List "body" (AttrList [("class", AString "container")]) [Unquote "content"]
      ]
    ]
  ]

doc :: Document
doc =
  Document
    (Config "Home page" (Just "/static/custom.css") "default")
    [ MacroCall "rounded-box"
      [ MacroArg "style" [String "beautiful"]
      , MacroArg "content" [String "hello world"]
      ]
    ]

docContent :: [Content]
docContent = let (Document _ content) = doc in content

macros :: [Macro]
macros =
  [ Macro "rounded-box"
    [ List "div" (AttrList [("class", AString "rounded"), ("style", AUnquote "style")])
      [Unquote "content"]
    ]
  ]

documentSpec :: SpecWith ()
documentSpec = describe "Generic documents" $ do
  it "should expand a simple layout" $ do
    applyLayout layouts doc `shouldBe` Just (
      [ List "html" (AttrList [])
        [ List "head" (AttrList [])
          [ List "pagetitle" (AttrList []) [String "Home page"]
          , List "link_" (AttrList [("rel", AString "stylesheet"), ("href", AString "/static/custom.css")]) []
          ]
        , List "body" (AttrList [("class", AString "container")])
          [ MacroCall "rounded-box"
            [ MacroArg "style" [String "beautiful"]
            , MacroArg "content" [String "hello world"]
            ]
          ]
        ]
      ] )

  it "should expand all macros in the body content" $ do
    expandAll macros docContent `shouldBe`
      [ List "div" (AttrList [("class", AString "rounded"), ("style", AString "beautiful")])
        [String "hello world"]
      ]

  it "should expand both macros and layouts" $ do
    (expandAll macros <$> applyLayout layouts doc) `shouldBe` Just (
      [ List "html" (AttrList [])
        [ List "head" (AttrList [])
          [ List "pagetitle" (AttrList []) [String "Home page"]
          , List "link_" (AttrList [("rel", AString "stylesheet"), ("href", AString "/static/custom.css")]) []
          ]
        , List "body" (AttrList [("class", AString "container")])
          [ List "div" (AttrList [("class", AString "rounded"), ("style", AString "beautiful")])
            [String "hello world"]
          ]
        ]
      ] )
