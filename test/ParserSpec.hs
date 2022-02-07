{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (parserSpec) where

import           Data.Either
import           Parser
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
      parse identifier "" "hello@" `shouldParse` "hello@"
      parse identifier "" "*a-cool-identifier*" `shouldParse` "*a-cool-identifier*"
      parse identifier "" "ident(ifier)" `shouldParse` "ident"
      parse identifier "" "" `shouldSatisfy` isLeft
      parse identifier "" "\n" `shouldSatisfy` isLeft
      parse identifier "" "(identifierfier)" `shouldSatisfy` isLeft
