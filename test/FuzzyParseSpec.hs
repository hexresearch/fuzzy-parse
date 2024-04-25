{-# LANGUAGE   OverloadedStrings
             , QuasiQuotes
             , ExtendedDefaultRules
             , LambdaCase
             , ImportQualifiedPost
             , DerivingStrategies
             , PatternSynonyms
             , ViewPatterns
#-}

module FuzzyParseSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Text.InterpolatedString.Perl6 (q)

import Data.Function
import Data.Functor
import Data.Text.Fuzzy.Tokenize
import Data.List qualified  as L
import Data.Either
import Control.Monad.Reader
import Data.Map qualified as Map
import Data.Typeable
import Control.Exception
import Control.Monad.Except
import Data.Maybe

data TTok = TChar Char
          | TSChar Char
          | TPunct Char
          | TText Text
          | TStrLit Text
          | TKeyword Text
          | TEmpty
          | TIndent Int
          deriving(Eq,Ord,Show)

instance IsToken TTok where
  mkChar = TChar
  mkSChar = TSChar
  mkPunct = TPunct
  mkText = TText
  mkStrLit = TStrLit
  mkKeyword = TKeyword
  mkEmpty = TEmpty
  mkIndent = TIndent


data SExpParseError =
    ParensOver
  | ParensUnder
  | ParensUnmatched
  deriving stock (Show,Typeable)


data MicroSexp =
    List   [MicroSexp]
  | Symbol Text
  | String Text
  deriving stock (Show)


nil :: MicroSexp
nil = List []

symbol :: Text -> MicroSexp
symbol = Symbol

str :: Text -> MicroSexp
str = String

tokenizeSexp :: Text -> [TTok]
tokenizeSexp txt =  do
  let spec = delims " \t" <> comment ";"
                          <> punct "{}()[]\n\r"
                          <> sq <> sqq
                          <> uw
  tokenize spec txt

parseSexp :: MonadError SExpParseError m => Text -> m MicroSexp
parseSexp txt = do
  tokenizeSexp txt & sexp <&> fst


sexp :: MonadError SExpParseError m => [TTok] -> m (MicroSexp, [TTok])
sexp s = case filtered s of
  [] -> pure (nil, mempty)
  (TText s : w) -> pure (Symbol s, w)

  (TStrLit s : w) -> pure (String s, w)

  (TPunct c : rest) | isBrace c  ->
    maybe (pure (nil, rest)) (`list` rest) (closing c)

                    | otherwise -> error (show c) -- throwError ParensOver

  where
    filtered xs = flip filter xs $ \case
      TPunct '\r' -> False
      TPunct '\n' -> False
      _           -> True

    isBrace c = Map.member c braces

    closing c = Map.lookup c braces

    isClosing = isJust . closing

    braces = Map.fromList[ ('{', '}')
                         , ('(', ')')
                         , ('[', ']')
                         ]

    -- list :: (Monad m, MonadError SExpParseError m) => Char -> [TTok] -> m (MicroSexp, [TTok])
    list c = go c mempty
      where

        isClosing :: Char -> Bool
        isClosing c = c `elem` ")}]"

        go cl acc [] = pure (List mempty, mempty)

        go cl acc (TPunct c : rest) | isClosing c && c == cl = pure (List acc, rest)
                                    | isClosing c && c /= cl = throwError ParensUnmatched

        go cl acc rest = do
          (e,r) <- sexp rest
          go cl (acc <> [e]) r

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


    describe "Checks indentation support" $ do

      let spec = delims " \n\t" <> comment ";"
                                <> punct "{}()[]<>"
                                <> sq <> sqq
                                <> uw
                                <> indent
                                <> itabstops 8
                                <> keywords ["define"]



      it "parses some indented blocks" $ do

        let expected = [ TIndent 0, TKeyword "define", TText "a", TText "0"
                       , TIndent 2, TText "atom", TText "foo", TText "2"
                       , TIndent 2, TKeyword "define", TText "aq", TText "2"
                       , TIndent 4, TText "atom", TText "one", TText "4"
                       , TIndent 4, TText "atom", TText "two", TText "4"
                       , TIndent 0, TKeyword "define", TText "b", TText "0"
                       , TIndent 2, TText "atom", TText "baar", TText "2"
                       , TIndent 2, TText "atom", TText "quux", TText "2"
                       , TIndent 2, TKeyword "define", TText "new", TText "2"
                       , TIndent 6, TText "atom", TText "bar", TText "6"
                       , TIndent 4, TText "atom", TText "fuu", TText "4"
                       , TIndent 0
                       ]

        let pyLike = [q|
define a      0
  atom foo    2
  define aq   2
    atom one  4
    atom two  4

define  b       0
  atom baar     2
  atom quux     2
  define new    2
      atom bar  6
    atom fuu    4

|]
        let toks = tokenize spec pyLike :: [TTok]
        toks `shouldBe` expected


