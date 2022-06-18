module ParserSpec (parserSpec) where

import           Data.Either
import           Data.Text             (Text)
import           Document
import           Macro
import           Parser
import           Parser.Internal
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec

parseWith :: Env -> Parser a -> Text -> Either ParserError a
parseWith env p = parseWithEnv env p ""

parserSpec :: SpecWith ()
parserSpec =
  describe "Parser" $ do
    let parse p = parseWithEnv
                    (mkEnv
                      ["par", "nl", "a", "b", "c", "i"]
                      ["class", "required", "href"]
                      ["macro", "already-existing-macro"])
                    p
                    ""

    it "should parse string literals" $ do
      parse stringedLiteral "\"This is a string\""   `shouldParse` "This is a string"
      parse stringedLiteral "\"This is a (string)\"" `shouldParse` "This is a (string)"
      parse stringedLiteral "\"This is @a string\""  `shouldParse` "This is @a string"
      parse stringedLiteral "\"  string with spaces  \"  " `shouldParse` "  string with spaces  "
      parse stringedLiteral "" `shouldSatisfy` isLeft

    it "should parse identifiers" $ do
      parse identifier "hello" `shouldParse` "hello"
      parse identifier "hello@" `shouldParse` "hello"
      parse identifier "*a-cool-identifier*" `shouldParse` "*a-cool-identifier*"
      parse identifier "ident(ifier)" `shouldParse` "ident"
      parse identifier "" `shouldSatisfy` isLeft
      parse identifier "\n" `shouldSatisfy` isLeft
      parse identifier "(identifierfier)" `shouldSatisfy` isLeft

    it "should parse macros" $ do
      parse macro "'(how-are-you @title @content)" `shouldParse` Macro "how-are-you" [Unquote "title", Unquote "content"]
      parse macro "'(  {test}  @test)" `shouldParse` Macro "{test}" [Unquote "test"]
      parse macro "'()" `shouldSatisfy` isLeft
      parse macro "'(())" `shouldSatisfy` isLeft

    it "empty macros are a thing" $ do
      parse macro "'(hi)" `shouldParse` Macro "hi" []
      parse macro "'(#)" `shouldParse` Macro "#" []

    it "cannot define twice the same macro" $ do
      parse macro "'(already-existing-macro)" `shouldSatisfy` isLeft

    it "should parse unquoted stuff" $ do
      parse unquote "@param-name" `shouldParse` Unquote "param-name"
      parse unquote "@strange/name" `shouldParse` Unquote "strange/name"
      parse unquote "" `shouldSatisfy` isLeft
      parse unquote "not@valid" `shouldSatisfy` isLeft

    it "should parse lists" $ do
      parse listOrMacro "(nl)" `shouldParse` List "nl" [] []
      parse listOrMacro "(b \"string content\")" `shouldParse` List "b" [] [String "string content"]
      parse listOrMacro "(a (b (c @d)))" `shouldParse` List "a" [] [List "b" [] [List "c" [] [Unquote "d"]]]
      parse listOrMacro "(b\n    \"string content\"\n    (i \"nested\"))" `shouldParse` List "b" [] [String "string content", List "i" [] [String "nested"]]
      parse listOrMacro "(par [(class \"red\")] (nl))" `shouldParse` List "par" [("class", String "red")] [List "nl" [] []]
      parse listOrMacro "()" `shouldSatisfy` isLeft
      parse listOrMacro "[nl]" `shouldSatisfy` isLeft
      parse listOrMacro "(nl [class red])" `shouldSatisfy` isLeft

    it "should not parse list with invalid names" $ do
      parse listOrMacro "(invalid-list-name)" `shouldSatisfy` isLeft

    it "macro bodies can bypass the list of valid names" $ do
      parse listOrMacro "(macro (invalid-list-name \"Hello\"))" `shouldParse` MacroCall "macro" [MacroArg "invalid-list-name" [String "Hello"]]
      parse listOrMacro "(par (macro (this-is-ok) (this-too (this-is-not))))" `shouldSatisfy` isLeft

    it "can parse multiple macros updating the environment" $ do
      parse (macro *> listOrMacro) "'(new-macro) (new-macro)" `shouldParse` MacroCall "new-macro" []
      parse (listOrMacro <* macro) "(new-macro) '(new-macro)" `shouldSatisfy` isLeft
      parse (many macro *> many listOrMacro) "'(new-macro) '(another-one) (new-macro) (another-one)" `shouldParse` [MacroCall "new-macro" [], MacroCall "another-one" []]

    it "should parse attribute lists" $ do
      parse attrList "[(class \"red\") (required)]" `shouldParse` [("class", String "red"), ("required", String "")]
      parse attrList "[(class \"red\") (href @linkValue)]" `shouldParse` [("class", String "red"), ("href", Unquote "linkValue")]
      parse attrList "[]" `shouldParse` []
      parse attrList "[(class (nl))]" `shouldSatisfy` isLeft
      parse attrList "[()]" `shouldSatisfy` isLeft
      parse attrList "[(class ?a)]" `shouldSatisfy` isLeft

    it "should not parse attribute lists with invalid keys" $ do
      parse attrList "[(class @valid) (invalid-name)]" `shouldSatisfy` isLeft

    it "should parse correct configs" $ do
      parse config "{ title \"Title\" custom-css \"/custom.css\" layout \"fancy\" }" `shouldParse` Config "Title" (Just "/custom.css") "fancy"
      parse config "{ title \"Title\" layout \"fancy\" }" `shouldParse` Config "Title" Nothing "fancy"
      parse config "{ title \"Title\" }" `shouldParse` Config "Title" Nothing "default"
      parse config "{ layout \"fancy\" custom-css \"/custom.css\" title \"Title\" }" `shouldParse` Config "Title" (Just "/custom.css") "fancy"
      parse config "{}" `shouldSatisfy` isLeft
