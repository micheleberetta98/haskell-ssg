module ParserSpec (parserSpec) where

import           Data.Either
import           Document
import           Macro
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
      parse identifier "" "*a-cool-identifier*" `shouldParse` "*a-cool-identifier*"
      parse identifier "" "ident(ifier)" `shouldParse` "ident"
      parse identifier "" "" `shouldSatisfy` isLeft
      parse identifier "" "\n" `shouldSatisfy` isLeft
      parse identifier "" "(identifierfier)" `shouldSatisfy` isLeft

    it "should parse macros" $ do
      parse macro "" "(macro hi)" `shouldParse` Macro "hi" []
      parse macro "" "(macro how-are-you @title @content)" `shouldParse` Macro "how-are-you" [Unquote "title", Unquote "content"]
      parse macro "" "(Macro hi)" `shouldSatisfy` isLeft
      parse macro "" "(macro)" `shouldSatisfy` isLeft
      parse macro "" "(macro ())" `shouldSatisfy` isLeft
      parse macro "" "(Macro {test})" `shouldSatisfy` isLeft

    it "empty macros are a thing" $ do
      parse macro "" "(macro #)" `shouldParse` Macro "#" []

    it "should parse unquoted stuff" $ do
      parse unquote "" "@param-name" `shouldParse` Unquote "param-name"
      parse unquote "" "@strange/name" `shouldParse` Unquote "strange/name"
      parse unquote "" "" `shouldSatisfy` isLeft
      parse unquote "" "not@valid" `shouldSatisfy` isLeft

    it "should parse lists" $ do
      parse list "" "(nl)" `shouldParse` List "nl" [] []
      parse list "" "(b \"string content\")" `shouldParse` List "b" [] [String "string content"]
      parse list "" "(a (b (c @d)))" `shouldParse` List "a" [] [List "b" [] [List "c" [] [Unquote "d"]]]
      parse list "" "(b\n    \"string content\"\n    (i \"nested\"))" `shouldParse` List "b" [] [String "string content", List "i" [] [String "nested"]]
      parse list "" "(par [(class \"red\")] (nl))" `shouldParse` List "par" [("class", String "red")] [List "nl" [] []]
      parse list "" "()" `shouldSatisfy` isLeft
      parse list "" "[nl]" `shouldSatisfy` isLeft
      parse list "" "(nl [class red])" `shouldSatisfy` isLeft

    it "should parse attribute lists" $ do
      parse attrList "" "[(class \"red\") (required)]" `shouldParse` [("class", String "red"), ("required", String "")]
      parse attrList "" "[(class \"red\") (href @linkValue)]" `shouldParse` [("class", String "red"), ("href", Unquote "linkValue")]
      parse attrList "" "[]" `shouldParse` []
      parse attrList "" "[(class (nl))]" `shouldSatisfy` isLeft
      parse attrList "" "[()]" `shouldSatisfy` isLeft
      parse attrList "" "[(class ?a)]" `shouldSatisfy` isLeft

    it "should parse correct configs" $ do
      parse config "" "{ title \"Title\" custom-css \"/custom.css\" layout \"fancy\" }" `shouldParse` Config "Title" (Just "/custom.css") "fancy"
      parse config "" "{ title \"Title\" layout \"fancy\" }" `shouldParse` Config "Title" Nothing "fancy"
      parse config "" "{ title \"Title\" }" `shouldParse` Config "Title" Nothing "default"
      parse config "" "{ layout \"fancy\" custom-css \"/custom.css\" title \"Title\" }" `shouldParse` Config "Title" (Just "/custom.css") "fancy"
      parse config "" "{}" `shouldSatisfy` isLeft
