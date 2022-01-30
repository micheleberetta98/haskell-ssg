{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (parserSpec) where

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
