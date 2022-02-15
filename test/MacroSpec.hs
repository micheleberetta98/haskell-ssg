module MacroSpec (macroSpec) where

import           Data.Either
import           Document
import           Macro.Internal
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

macroSpec :: SpecWith ()
macroSpec =
  let l n = List n []
  in
  describe "Macro" $ do
    it "should substitue correctly" $ do
      let params = [("param1", [String "hello"]), ("param2", [String "goodbye"])]
      substitute params [Unquote "param1"] `shouldBe` [String "hello"]
      substitute params [Unquote "param2"] `shouldBe` [String "goodbye"]
      substitute params [l "nl" []] `shouldBe` [l "nl" []]
      substitute params [l "p" [Unquote "param1", Unquote "__"]] `shouldBe` [l "p" [String "hello"]]
      substitute params [l "p" [l "div" [], List "div" [] [Unquote "param2"]]] `shouldBe` [l "p" [l "div" [], l "div" [String "goodbye"]]]

    it "should expand in documents' content" $ do
      let m1 = Macro "m1" [l "div" [], l "p" [Unquote "content"]]
          m2 = Macro "m2" [l "p" [Unquote "content", l "nl" [], String "hello"]]
      let doc1 = l "m1" [l "content" [String "content1", String "content2"]]
          doc2 = l "m2" [l "content" [String "content1", String "content2"]]
      expand m1 [doc1] `shouldBe` [l "div" [], l "p" [String "content1", String "content2"]]
      expand m2 [doc1] `shouldBe` [doc1]
      expand m1 [doc2] `shouldBe` [doc2]
      expand m2 [doc2] `shouldBe` [l "p" [String "content1", String "content2", l "nl" [], String "hello"]]

    it "should expand in a list's attrlist" $ do
      let m = Macro "*macro*" [List "div" [("class", Unquote "class")] [Unquote "content"]]
      let doc = l "*macro*"
            [ l "content" [String "content1", String "content2"]
            , l "class" [String "red"]
            ]
      expand m [doc] `shouldBe` [List "div" [("class", String "red")] [String "content1", String "content2"]]

    it "should expand multiple macros" $ do
      let m1 = Macro "m1" [l "div" [], l "p" [Unquote "content"]]
          m2 = Macro "m2" [l "p" [Unquote "content", l "section" [Unquote "content"], String "hello"]]
      let doc = [l "m1" [l "content" [String "macro 1 content"]], l "m2" [l "content" [String "macro 2 content"]]]
      expandAll [m1, m2] doc `shouldBe`
        [ l "div" []
        , l "p" [String "macro 1 content"]
        , l "p"
          [ String "macro 2 content"
          , l "section" [String "macro 2 content"]
          , String "hello"
          ]
        ]
      expandAll [m1, m2] doc `shouldBe` expandAll [m2, m1] doc
      expandAll [m1, m1, m1] doc `shouldBe` expand m1 doc
