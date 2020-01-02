{-# LANGUAGE   OverloadedStrings
             , QuasiQuotes
             , ExtendedDefaultRules #-}

module FuzzyParseSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Text.InterpolatedString.Perl6 (q)

import Data.Text.Fuzzy.Tokenize

data TTok = TChar Char
          | TSChar Char
          | TPunct Char
          | TText Text
          | TStrLit Text
          | TKeyword Text
          | TEmpty
          deriving(Eq,Ord,Show)

instance IsToken TTok where
  mkChar = TChar
  mkSChar = TSChar
  mkPunct = TPunct
  mkText = TText
  mkStrLit = TStrLit
  mkKeyword = TKeyword
  mkEmpty = TEmpty

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


    it "tokenize simple lisp-like text with keywords" $ do
      let spec = delims " \n\t" <> comment ";"
                                <> punct "{}()[]<>"
                                <> sq <> sqq
                                <> uw
                                <> keywords ["define","apply","+"]

      let code = [q|
        (define add (a b ) ; define simple function
          (+ a b) )
        (define r (add 10 20))
|]

      let toks = tokenize spec code :: [TTok]

      let expected = [ TPunct '('
                     , TKeyword "define"
                     , TText "add" , TPunct '(', TText "a" , TText "b", TPunct ')'
                       , TPunct '(', TKeyword "+", TText "a",TText "b",TPunct ')',TPunct ')'
                     ,TPunct '(',TKeyword "define"
                                  ,TText "r"
                                  ,TPunct '(',TText "add",TText "10",TText "20"
                                  ,TPunct ')',TPunct ')']

      toks `shouldBe` expected

