module MacroSpec (macroSpec) where

import           Data.Either
import           Document
import           Macro.Internal
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

macroSpec :: SpecWith ()
macroSpec =
  describe "Macro" $ do
    it "should substitue correctly" $ do
      let params = [("param1", [String "hello"]), ("param2", [String "goodbye"])]
      substitute params [Unquote "param1"] `shouldBe` [String "hello"]
      substitute params [Unquote "param2"] `shouldBe` [String "goodbye"]
      substitute params [List "nl" []] `shouldBe` [List "nl" []]
      substitute params [List "p" [Unquote "param1", Unquote "__"]] `shouldBe` [List "p" [String "hello"]]
      substitute params [List "p" [List "div" [], List "div" [Unquote "param2"]]] `shouldBe` [List "p" [List "div" [], List "div" [String "goodbye"]]]

    it "should expand" $ do
      let m1 = Macro "m1" [List "div" [], List "p" [Unquote "content"]]
          m2 = Macro "m2" [List "p" [Unquote "content", List "nl" [], String "hello"]]
      let doc1 = List "m1" [List "content" [String "content1", String "content2"]]
          doc2 = List "m2" [List "content" [String "content1", String "content2"]]
      expand m1 [doc1] `shouldBe` [List "div" [], List "p" [String "content1", String "content2"]]
      expand m2 [doc1] `shouldBe` [doc1]
      expand m1 [doc2] `shouldBe` [doc2]
      expand m2 [doc2] `shouldBe` [List "p" [String "content1", String "content2", List "nl" [], String "hello"]]

    it "should expand multiple macros" $ do
      let m1 = Macro "m1" [List "div" [], List "p" [Unquote "content"]]
          m2 = Macro "m2" [List "p" [Unquote "content", List "section" [Unquote "content"], String "hello"]]
      let doc = [List "m1" [List "content" [String "macro 1 content"]], List "m2" [List "content" [String "macro 2 content"]]]
      expandAll [m1, m2] doc `shouldBe`
        [ List "div" []
        , List "p" [String "macro 1 content"]
        , List "p"
          [ String "macro 2 content"
          , List "section" [String "macro 2 content"]
          , String "hello"
          ]
        ]
      expandAll [m1, m2] doc `shouldBe` expandAll [m2, m1] doc
      expandAll [m1, m1, m1] doc `shouldBe` expand m1 doc
