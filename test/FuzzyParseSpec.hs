{-# LANGUAGE   OverloadedStrings
             , QuasiQuotes
             , ExtendedDefaultRules
             , LambdaCase
             , ImportQualifiedPost
             , DerivingStrategies
             , PatternSynonyms
             , ViewPatterns
             , MultiWayIf
             , TemplateHaskell
#-}

module FuzzyParseSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Text.InterpolatedString.Perl6 (q)

import Control.Applicative
import Data.Function
import Data.Functor
import Data.Text.Fuzzy.Tokenize
import Data.List qualified  as L
import Data.Either
import Control.Monad.Reader
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Typeable
import Control.Exception
import Control.Monad.Except
import Control.Monad.RWS
import Data.Maybe
import Data.Generics.Uniplate.Operations
import Data.Data
import Data.Char (isSpace)
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import GHC.Generics()
import Safe
import Data.Data
import GHC.Generics
import Data.Generics.Uniplate.Operations
import Lens.Micro.Platform
import Data.Text qualified as Text
import Data.Foldable
import Data.Traversable
import Debug.Trace

import Streaming.Prelude qualified as S

data TTok = TChar Char
          | TSChar Char
          | TPunct Char
          | TText Text
          | TStrLit Text
          | TKeyword Text
          | TEmpty
          | TIndent Int
          deriving stock (Eq,Ord,Show,Data,Generic)

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


data NumType =
    NumInteger Integer
  | NumDouble  Double
  deriving stock (Eq,Ord,Show,Data,Generic)

data MicroSexp =
    List    [MicroSexp]
  | Symbol  Text
  | String  Text
  | Number  NumType
  | Boolean Bool
  deriving stock (Show,Data,Generic)

nil :: MicroSexp
nil = List []

symbol :: Text -> MicroSexp
symbol = Symbol

str :: Text -> MicroSexp
str = String

newtype SExpEnv =
  SExpEnv
  { sexpTranslate :: Bool
  }

data SExpState =
  SExpState
  { _sexpLno    :: Int
  , _sexpBraces :: [Char]
  }

makeLenses 'SExpState

defEnv :: SExpEnv
defEnv = SExpEnv True

newtype SExpM m a = SExpM { fromSexpM :: RWST SExpEnv () SExpState m a }
                    deriving newtype
                      ( Applicative
                      , Functor
                      , Monad
                      , MonadState SExpState
                      , MonadReader SExpEnv
                      , MonadTrans
                      )



tokenizeSexp :: Text -> [TTok]
tokenizeSexp txt =  do
  let spec = delims " \t" <> comment ";"
                          <> punct "{}()[]\n\r"
                          <> sq <> sqq
                          <> uw
  tokenize spec txt

runSexpM :: Monad m => SExpM m a -> m a
runSexpM f = evalRWST (fromSexpM f) defEnv (SExpState 0 []) <&> fst


isBrace :: Char -> Bool
isBrace c = Map.member c braces

closing :: Char -> Maybe Char
closing c = Map.lookup c braces

isClosing :: Char -> Bool
isClosing = isJust . closing

braces :: Map Char Char
braces = Map.fromList[ ('{', '}')
                     , ('(', ')')
                     , ('[', ']')
                     , ('<', '>')
                     ]



parseSexp :: (MonadIO m, MonadError SExpParseError m) => Text -> m MicroSexp
parseSexp txt = do
  (s, rest) <- runSexpM do
                (s,rest) <- sexp (tokenizeSexp txt)
                when (null rest) do
                  braces <- gets (view sexpBraces)
                  unless (null braces) $ lift $ throwError ParensUnder
                pure (s,rest)

  pure s

parseTop :: MonadError SExpParseError m => Text -> m [MicroSexp]
parseTop txt = do
  let tokens = tokenizeSexp txt
  r <- S.toList_ $ runSexpM do
          flip fix (mempty,tokens) $ \next -> \case
            (acc, []) -> do
              traceM  "JOPA?"
              emit acc

            (acc, TPunct '\n' : rest) -> do
              emit acc
              traceM  "KITA?"
              next (mempty,rest)
            (acc, rest) -> do
              (s, xs) <- sexp rest
              traceM ( "PECHEN?" <> show (s,xs))
              next (acc <> [s],xs)

  pure $ unlist r

  where

    emit [] = pure ()
    emit x  = lift $ S.yield (List x)

    unlist = \case
      [List xs] -> xs
      other     -> other

sexp :: MonadError SExpParseError m => [TTok] -> SExpM m (MicroSexp, [TTok])
sexp s = case s of
  [] -> pure (nil, mempty)
  (TText s : w) -> transformBiM trNum (Symbol s, w)

  (TStrLit s : w) -> pure (String s, w)

  (TPunct c : rest) | isSpace c  -> sexp rest

  (TPunct c : rest) | isBrace c  ->
    maybe (pure (nil, rest)) (`list` rest) (closing c)
                    | otherwise -> do
                        lift $ throwError ParensOver

  where

    trNum tok = do

      trans <- asks sexpTranslate

      case tok of
        Symbol s | trans -> do
          let s0 = Text.unpack s

          let what = Number . NumInteger <$> readMay @Integer s0
                    <|>
                    Number . NumDouble <$> readMay @Double s0
                    <|>
                    ( if | s == "#t"  -> (Just (Boolean True) )
                         | s == "#f"  -> (Just (Boolean False) )
                         | otherwise  -> Nothing
                    )

          pure $ fromMaybe (Symbol s) what


        x        -> pure x

    filtered xs = flip filter xs $ \case
      TPunct '\r' -> False
      TPunct '\n' -> False
      _           -> True


    list :: (MonadError SExpParseError m) => Char -> [TTok] -> SExpM m (MicroSexp, [TTok])
    list c tokens = do
      modify $ over sexpBraces (c:)

      go c mempty tokens

      where

        isClosing :: Char -> Bool
        isClosing c = c `elem` ")}]"

        go cl acc [] = pure (List mempty, mempty)

        go cl acc (TPunct c : rest)
          | isClosing c && c == cl = do
              modify $ over sexpBraces (drop 1)
              pure (List acc, rest)

          | isClosing c && c /= cl = do
              lift $ throwError ParensUnmatched

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
                     , TPunct '(',TKeyword "define"
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


