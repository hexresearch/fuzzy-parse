{-# LANGUAGE   OverloadedStrings
             , QuasiQuotes
             , ExtendedDefaultRules #-}

module FuzzyParseSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Text.InterpolatedString.Perl6 (qc)

import Data.Text.Fuzzy.Tokenize


spec :: Spec
spec = do
  describe "csv-like" $ do
    it "splits text using ':' delimeter" $ do
      let toks = tokenize (delims ":") "aaa : bebeb : qqq ::::" :: [Text]
      toks `shouldBe` ["aaa "," bebeb "," qqq "]

    it "splits text using ':' delimeter with single-quotes string and empty fields" $ do
      let toks = tokenize (delims ":"<>sq<>emptyFields ) "aaa : bebeb : qqq ::::" :: [Text]
      toks `shouldBe` ["aaa "," bebeb "," qqq ","","","",""]

    it "splits text using ':' delimeter with single-quotes string and empty fields" $ do
      let toks = tokenize (delims ":"<>sq<>emptyFields ) "aaa : 'bebeb:colon inside' : qqq ::::" :: [Maybe Text]
      toks `shouldBe` [Just "aaa ",Just " ",Just "bebeb:colon inside",Just " ",Just " qqq ",Nothing,Nothing,Nothing,Nothing]

    it "splits text using ':' delimeter with single-quotes string and empty fields with noslits" $ do
      let spec = sl<>delims ":"<>sq<>emptyFields<>noslits
      let toks =  tokenize spec "   aaa :   'bebeb:colon inside' : qqq ::::" :: [Maybe Text]
      toks `shouldBe` [Just "aaa ",Just "bebeb:colon inside ",Just "qqq ",Nothing,Nothing,Nothing,Nothing]

    it "splits text using ':' delimeter with single-quotes string and empty fields with noslits and uw" $ do
      let spec = delims ":"<>sq<>emptyFields<>uw<>noslits
      let toks = tokenize spec "  a  b  c  : 'bebeb:colon inside' : qqq ::::"  :: [Maybe Text]
      toks `shouldBe` [Just "a b c",Just "bebeb:colon inside",Just "qqq",Nothing,Nothing,Nothing,Nothing]

    it "uses punctuation tokens" $ do
      let spec = delims " \t"<>punct ",;()" <>emptyFields<>sq
      let toks = tokenize spec "( delimeters , are , important, 'spaces are not');" :: [Text]
      toks `shouldBe` ["(","delimeters",",","are",",","important",",","spaces are not",")",";"]


