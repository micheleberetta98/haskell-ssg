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
    [ List "html" []
      [ List "head" []
        [ List "pagetitle" [] [Unquote "pageTitle"]
        , List "link_" [("rel", String "stylesheet"), ("href", Unquote "customCss")] []
        ]
      , List "body" [("class", String "container")] [Unquote "content"]
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
    [ List "div" [("class", String "rounded"), ("style", Unquote "style")]
      [Unquote "content"]
    ]
  ]

documentSpec :: SpecWith ()
documentSpec = describe "Generic documents" $ do
  it "should expand a simple layout" $ do
    applyLayout layouts doc `shouldBe` Just (
      [ List "html" []
        [ List "head" []
          [ List "pagetitle" [] [String "Home page"]
          , List "link_" [("rel", String "stylesheet"), ("href", String "/static/custom.css")] []
          ]
        , List "body" [("class", String "container")]
          [ MacroCall "rounded-box"
            [ MacroArg "style" [String "beautiful"]
            , MacroArg "content" [String "hello world"]
            ]
          ]
        ]
      ] )

  it "should expand all macros in the body content" $ do
    expandAll macros docContent `shouldBe`
      [ List "div" [("class", String "rounded"), ("style", String "beautiful")]
        [String "hello world"]
      ]

  it "should expand both macros and layouts" $ do
    (expandAll macros <$> applyLayout layouts doc) `shouldBe` Just (
      [ List "html" []
        [ List "head" []
          [ List "pagetitle" [] [String "Home page"]
          , List "link_" [("rel", String "stylesheet"), ("href", String "/static/custom.css")] []
          ]
        , List "body" [("class", String "container")]
          [ List "div" [("class", String "rounded"), ("style", String "beautiful")]
            [String "hello world"]
          ]
        ]
      ] )
