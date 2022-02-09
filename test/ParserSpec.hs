{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (parserSpec) where

import           Data.Either
import           Document
import           Parser.Internal
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

parserSpec :: SpecWith ()
parserSpec =
  describe "Parser" $ do
    it "should parse string literals" $ do
      parse stringedLiteral "" "\"This is a string\""   `shouldParse` "This is a string"
      parse stringedLiteral "" "\"This is a (string)\"" `shouldParse` "This is a (string)"
      parse stringedLiteral "" "\"This is @a string\""  `shouldParse` "This is @a string"
      parse stringedLiteral "" "\"  string with spaces  \"  " `shouldParse` "  string with spaces  "
      parse stringedLiteral "" "" `shouldSatisfy` isLeft

    it "should parse identifiers" $ do
      parse identifier "" "hello" `shouldParse` "hello"
      parse identifier "" "hello@" `shouldParse` "hello"
      parse identifier "" "a-cool-identifier*" `shouldParse` "a-cool-identifier*"
      parse identifier "" "ident(ifier)" `shouldParse` "ident"
      parse identifier "" "" `shouldSatisfy` isLeft
      parse identifier "" "\n" `shouldSatisfy` isLeft
      parse identifier "" "(identifierfier)" `shouldSatisfy` isLeft

    it "should parse unquotes" $ do
      parse unquote "" "@param-name" `shouldParse` Unquote "param-name"
      parse unquote "" "@strange/name" `shouldParse` Unquote "strange/name"
      parse unquote "" "" `shouldSatisfy` isLeft
      parse unquote "" "not@valid" `shouldSatisfy` isLeft

    it "should parse blocks" $ do
      let ebl n = Block n (AttrList []) []
          bl n = Block n (AttrList [])

      parse block "" "(nl)" `shouldParse` ebl "nl"
      parse block "" "(nl [])" `shouldParse` ebl "nl"
      parse block "" "(b \"string content\")" `shouldParse` bl "b" [String "string content"]
      parse block "" "(a (b (c @d)))" `shouldParse` bl "a" [bl "b" [bl "c" [Unquote "d"]]]
      parse block "" "(par [class \"red\"] (nl))" `shouldParse` Block "par" (AttrList [("class", "red")]) [ebl "nl"]
      parse block "" "(b\n    \"string content\"\n    (i \"nested\"))" `shouldParse` bl "b" [String "string content", bl "i" [String "nested"]]
      parse block "" "[nl]" `shouldSatisfy` isLeft
      parse block "" "(nl [class red])" `shouldSatisfy` isLeft
      parse block "" "()" `shouldSatisfy` isLeft

    it "should parse correct configs" $ do
      parse config "" "{ title \"Title\" custom-css \"/custom.css\" layout \"fancy\" }" `shouldParse` Config "Title" (Just "/custom.css") "fancy"
      parse config "" "{ title \"Title\" layout \"fancy\" }" `shouldParse` Config "Title" Nothing "fancy"
      parse config "" "{ title \"Title\" }" `shouldParse` Config "Title" Nothing "default"
      parse config "" "{ layout \"fancy\" custom-css \"/custom.css\" title \"Title\" }" `shouldParse` Config "Title" (Just "/custom.css") "fancy"
      parse config "" "{}" `shouldSatisfy` isLeft
