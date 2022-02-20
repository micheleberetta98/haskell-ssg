module ParserSpec (parserSpec) where

import           Data.Either
import           Document
import           Macro
import           Parser.Env
import           Parser.Internal
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

parserSpec :: SpecWith ()
parserSpec =
  describe "Parser" $ do
    let env = mkEnv
                ["par", "nl", "a", "b", "c", "i"]
                ["class", "required", "href"]
                ["already-existing-macro"]
        macroWithEnv = macro env
        listWithEnv = list env
        attrListWithEnv = attrList env
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
      parse macroWithEnv "" "(macro hi)" `shouldParse` Macro "hi" []
      parse macroWithEnv "" "(macro how-are-you @title @content)" `shouldParse` Macro "how-are-you" [Unquote "title", Unquote "content"]
      parse macroWithEnv "" "(Macro hi)" `shouldSatisfy` isLeft
      parse macroWithEnv "" "(macro)" `shouldSatisfy` isLeft
      parse macroWithEnv "" "(macro ())" `shouldSatisfy` isLeft
      parse macroWithEnv "" "(Macro {test})" `shouldSatisfy` isLeft

    it "empty macros are a thing" $ do
      parse macroWithEnv "" "(macro #)" `shouldParse` Macro "#" []

    it "cannot definte twice the same macro" $ do
      parse macroWithEnv "" "(macro already-existing-macro)" `shouldSatisfy` isLeft

    it "should parse unquoted stuff" $ do
      parse unquote "" "@param-name" `shouldParse` Unquote "param-name"
      parse unquote "" "@strange/name" `shouldParse` Unquote "strange/name"
      parse unquote "" "" `shouldSatisfy` isLeft
      parse unquote "" "not@valid" `shouldSatisfy` isLeft

    it "should parse lists" $ do
      parse listWithEnv "" "(nl)" `shouldParse` List "nl" [] []
      parse listWithEnv "" "(b \"string content\")" `shouldParse` List "b" [] [String "string content"]
      parse listWithEnv "" "(a (b (c @d)))" `shouldParse` List "a" [] [List "b" [] [List "c" [] [Unquote "d"]]]
      parse listWithEnv "" "(b\n    \"string content\"\n    (i \"nested\"))" `shouldParse` List "b" [] [String "string content", List "i" [] [String "nested"]]
      parse listWithEnv "" "(par [(class \"red\")] (nl))" `shouldParse` List "par" [("class", String "red")] [List "nl" [] []]
      parse listWithEnv "" "()" `shouldSatisfy` isLeft
      parse listWithEnv "" "[nl]" `shouldSatisfy` isLeft
      parse listWithEnv "" "(nl [class red])" `shouldSatisfy` isLeft

    it "should not parse list with invalid names" $ do
      parse listWithEnv "" "(invalid-list-name)" `shouldSatisfy` isLeft

    it "macro bodies can bypass the list of valid names" $ do
      parse listWithEnv "" "(already-existing-macro (invalid-list-name \"Hello\"))" `shouldParse` List "already-existing-macro" [] [List "invalid-list-name" [] [String "Hello"]]


    it "should parse attribute lists" $ do
      parse attrListWithEnv "" "[(class \"red\") (required)]" `shouldParse` [("class", String "red"), ("required", String "")]
      parse attrListWithEnv "" "[(class \"red\") (href @linkValue)]" `shouldParse` [("class", String "red"), ("href", Unquote "linkValue")]
      parse attrListWithEnv "" "[]" `shouldParse` []
      parse attrListWithEnv "" "[(class (nl))]" `shouldSatisfy` isLeft
      parse attrListWithEnv "" "[()]" `shouldSatisfy` isLeft
      parse attrListWithEnv "" "[(class ?a)]" `shouldSatisfy` isLeft

    it "should not parse attribute lists with invalid keys" $ do
      parse attrListWithEnv "" "[(class @valid) (invalid-name)]" `shouldSatisfy` isLeft

    it "should parse correct configs" $ do
      parse config "" "{ title \"Title\" custom-css \"/custom.css\" layout \"fancy\" }" `shouldParse` Config "Title" (Just "/custom.css") "fancy"
      parse config "" "{ title \"Title\" layout \"fancy\" }" `shouldParse` Config "Title" Nothing "fancy"
      parse config "" "{ title \"Title\" }" `shouldParse` Config "Title" Nothing "default"
      parse config "" "{ layout \"fancy\" custom-css \"/custom.css\" title \"Title\" }" `shouldParse` Config "Title" (Just "/custom.css") "fancy"
      parse config "" "{}" `shouldSatisfy` isLeft
