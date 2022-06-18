module MacroSpec (macroSpec) where

import           Document
import           Macro.Internal
import           Test.Hspec

macroSpec :: SpecWith ()
macroSpec =
  let l n = List n []
      l_ n = l n []
  in
  describe "Macro" $ do
    it "should substitue correctly" $ do
      let params = [ MacroArg "param1" [String "hello"]
                   , MacroArg "param2" [String "goodbye"]
                   ]
      substitute params [Unquote "param1"] `shouldBe` [String "hello"]
      substitute params [Unquote "param2"] `shouldBe` [String "goodbye"]
      substitute params [l "nl" []] `shouldBe` [l "nl" []]
      substitute params [l "p" [Unquote "param1", Unquote "__"]] `shouldBe` [l "p" [String "hello"]]
      substitute params [l "p" [l_ "div", List "div" [] [Unquote "param2"]]] `shouldBe` [l "p" [l_ "div", l "div" [String "goodbye"]]]

    it "should expand in documents' content" $ do
      let m1 = Macro "m1" [l_ "div", l "p" [Unquote "content"]]
          m2 = Macro "m2" [l "p" [Unquote "content", l "nl" [], String "hello"]]
      let doc1 = MacroCall "m1" [MacroArg "content" [String "content1", String "content2"]]
          doc2 = MacroCall "m2" [MacroArg "content" [String "content1", String "content2"]]

      expand m1 [doc1] `shouldBe` [l_ "div", l "p" [String "content1", String "content2"]]
      expand m2 [doc1] `shouldBe` [doc1]
      expand m1 [doc2] `shouldBe` [doc2]
      expand m2 [doc2] `shouldBe` [l "p" [String "content1", String "content2", l "nl" [], String "hello"]]

    it "should expand in a list's attrlist" $ do
      let m = Macro "*macro*" [List "div" [("class", Unquote "class")] [Unquote "content"]]
      let doc = MacroCall "*macro*"
            [ MacroArg "content" [String "content1", String "content2"]
            , MacroArg "class" [String "red"]
            ]

      expand m [doc] `shouldBe` [List "div" [("class", String "red")] [String "content1", String "content2"]]

    it "should expand multiple macros" $ do
      let m1 = Macro "*M1*" [ List "div"
                              [("class", String "rounded"), ("style", Unquote "style")]
                              [Unquote "content"]
                            ]

          m2 = Macro "*M2*" [List "p" [] [Unquote "content", List "section" [] [Unquote "content"], String "hello"]]

      let doc = [ MacroCall "*M1*" [MacroArg "style" [String "beautiful"], MacroArg "content" [String "macro 1 content"]]
                , MacroCall "*M2*" [MacroArg "content" [String "macro 2 content"]]]

      expandAll [m1, m2] doc `shouldBe`
        [ List "div"
          [("class", String "rounded"), ("style", String "beautiful")]
          [String "macro 1 content"]
        , List "p" []
          [ String "macro 2 content"
          , List "section" [] [String "macro 2 content"]
          , String "hello"
          ]
        ]
      expandAll [m1, m2] doc `shouldBe` expandAll [m2, m1] doc
      expandAll [m1, m1, m1] doc `shouldBe` expand m1 doc

    it "should expand macros calling other macros" $ do
      let m1 = Macro "m1" [MacroCall "H" [MacroArg "title" [String "Macro 1"]], Unquote "content"]
          m2 = Macro "H" [l "h1" [Unquote "title"]]

      let doc = [MacroCall "m1" [MacroArg "content" [String "Hello"]]]

      expandAll [m1, m2] doc `shouldBe` [l "h1" [String "Macro 1"], String "Hello"]
      expandAll [m2, m1] doc `shouldBe` [l "h1" [String "Macro 1"], String "Hello"]

    it "should expand recursively" $ do
      let m = Macro "MACRO" [Unquote "content"]
          doc = [List "div" [] [MacroCall "MACRO" [MacroArg "content" [String "Hello"]]]]

      expandAll [m] doc `shouldBe` [List "div" [] [String "Hello"]]
