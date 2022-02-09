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
    let lst n = List n (AttrList [])
        lst' n = List n (AttrList [("class", "red")])

    it "should substitue correctly" $ do
      let params = [("param1", [String "hello"]), ("param2", [String "goodbye"])]
      substitute params [Unquote "param1"] `shouldBe` [String "hello"]
      substitute params [Unquote "param2"] `shouldBe` [String "goodbye"]
      substitute params [lst "nl" []] `shouldBe` [lst "nl" []]
      substitute params [lst "p" [Unquote "param1", Unquote "__"]] `shouldBe` [lst "p" [String "hello"]]
      substitute params [lst' "p" [lst' "div" [], lst' "div" [Unquote "param2"]]] `shouldBe` [lst' "p" [lst' "div" [], lst' "div" [String "goodbye"]]]

    it "should expand" $ do
      let m1 = Macro "m1" [lst "div" [], lst "p" [Unquote "content"]]
          m2 = Macro "m2" [lst "p" [Unquote "content", lst "nl" [], String "hello"]]
      let doc1 = lst "m1" [lst "content" [String "content1", String "content2"]]
          doc2 = lst "m2" [lst "content" [String "content1", String "content2"]]
      expand m1 [doc1] `shouldBe` [lst "div" [], lst "p" [String "content1", String "content2"]]
      expand m2 [doc1] `shouldBe` [doc1]
      expand m1 [doc2] `shouldBe` [doc2]
      expand m2 [doc2] `shouldBe` [lst "p" [String "content1", String "content2", lst "nl" [], String "hello"]]

