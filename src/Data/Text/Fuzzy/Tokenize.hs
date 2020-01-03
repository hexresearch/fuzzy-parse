-- |
-- Module      :  Data.Text.Fuzzy.Tokenize
-- Copyright   :  Dmitry Zuikov 2020
-- License     :  MIT
--
-- Maintainer  :  dzuikov@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- The lightweight and multi-functional text tokenizer allowing different types of text tokenization
-- depending on it's settings.
--
-- It may be used in different sutiations, for DSL, text markups or even for parsing simple grammars
-- easier and sometimes faster than in case of usage mainstream parsing combinators or parser
-- generators.
--
-- The primary goal of this package  is to parse unstructured text data, however it may be  used for
-- parsing  such data formats as CSV with ease.
--
-- Currently it supports the following types of entities: atoms, string literals (currently with the
-- minimal set of escaped characters), punctuation characters and delimeters.
--
-- == Examples
-- === Simple CSV-like tokenization
-- >>> tokenize (delims ":") "aaa : bebeb : qqq ::::" :: [Text]
-- ["aaa "," bebeb "," qqq "]
--
-- >>> tokenize (delims ":"<>sq<>emptyFields ) "aaa : bebeb : qqq ::::" :: [Text]
-- ["aaa "," bebeb "," qqq ","","","",""]
--
-- >>>> tokenize (delims ":"<>sq<>emptyFields ) "aaa : bebeb : qqq ::::" :: [Maybe Text]
-- [Just "aaa ",Just " bebeb ",Just " qqq ",Nothing,Nothing,Nothing,Nothing]
--
-- >>> tokenize (delims ":"<>sq<>emptyFields ) "aaa : 'bebeb:colon inside' : qqq ::::" :: [Maybe Text]
-- [Just "aaa ",Just " ",Just "bebeb:colon inside",Just " ",Just " qqq ",Nothing,Nothing,Nothing,Nothing]
--
-- >>> let spec = sl<>delims ":"<>sq<>emptyFields<>noslits
-- >>> tokenize spec "   aaa :   'bebeb:colon inside' : qqq ::::" :: [Maybe Text]
-- [Just "aaa ",Just "bebeb:colon inside ",Just "qqq ",Nothing,Nothing,Nothing,Nothing]
--
-- >>> let spec = delims ":"<>sq<>emptyFields<>uw<>noslits
-- >>> tokenize spec "  a  b  c  : 'bebeb:colon inside' : qqq ::::"  :: [Maybe Text]
-- [Just "a b c",Just "bebeb:colon inside",Just "qqq",Nothing,Nothing,Nothing,Nothing]
--
-- == Notes
--
-- === About the delimeter tokens
-- This type of tokens appears during a "delimited"
-- formats processing and disappears in results. Currenly
-- you will never see it unless normalization is turned off by 'nn' option.
--
-- The delimeters make sense in case of processing the CSV-like formats,
-- but in this case you probably need only values in results.
--
-- This behavior may be changed later. But right now delimeters seem pointless
-- in results. If you process some sort of grammar where delimeter character
-- is important, you may use punctuation instead, i.e:
--
-- >>> let spec = delims " \t"<>punct ",;()" <>emptyFields<>sq
-- >>> tokenize spec "( delimeters , are , important, 'spaces are not');" :: [Text]
-- ["(","delimeters",",","are",",","important",",","spaces are not",")",";"]
--
-- == Other
-- For CSV-like formats it makes sense to split text to lines first,
-- otherwise newline characters may cause to weird results
--
--

module Data.Text.Fuzzy.Tokenize ( TokenizeSpec
                                , IsToken(..)
                                , tokenize
                                , esc
                                , addEmptyFields
                                , emptyFields
                                , nn
                                , sq
                                , sqq
                                , noslits
                                , sl
                                , sr
                                , uw
                                , delims
                                , comment
                                , punct
                                , keywords
                                ) where

import Prelude hiding (init)

import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.List as List
import Data.Monoid()
import Control.Applicative

import Control.Monad.RWS

-- | Tokenization settings. Use mempty for an empty value
-- and construction functions for changing the settings.
--
data TokenizeSpec = TokenizeSpec { tsAtoms          :: Set Text
                                 , tsStringQQ       :: Maybe Bool
                                 , tsStringQ        :: Maybe Bool
                                 , tsNoSlits        :: Maybe Bool
                                 , tsLineComment    :: Map Char Text
                                 , tsDelims         :: Set Char
                                 , tsStripLeft      :: Maybe Bool
                                 , tsStripRight     :: Maybe Bool
                                 , tsUW             :: Maybe Bool
                                 , tsNotNormalize   :: Maybe Bool
                                 , tsEsc            :: Maybe Bool
                                 , tsAddEmptyFields :: Maybe Bool
                                 , tsPunct          :: Set Char
                                 , tsKeywords       :: Set Text
                                 }
                    deriving (Eq,Ord,Show)


instance Semigroup TokenizeSpec where
  (<>) a b = TokenizeSpec { tsAtoms       = tsAtoms b <> tsAtoms a
                          , tsStringQQ    = tsStringQQ b <|> tsStringQQ a
                          , tsStringQ     = tsStringQ b  <|> tsStringQ a
                          , tsNoSlits     = tsNoSlits b <|> tsNoSlits a
                          , tsLineComment = tsLineComment b <> tsLineComment a
                          , tsDelims      = tsDelims b <> tsDelims a
                          , tsStripLeft   = tsStripLeft b <|> tsStripLeft a
                          , tsStripRight  = tsStripRight b <|> tsStripRight a
                          , tsUW          = tsUW b <|> tsUW a
                          , tsNotNormalize = tsNotNormalize b <|> tsNotNormalize a
                          , tsEsc         = tsEsc b <|> tsEsc a
                          , tsAddEmptyFields = tsAddEmptyFields b <|> tsAddEmptyFields a
                          , tsPunct = tsPunct b <> tsPunct a
                          , tsKeywords = tsKeywords b <> tsKeywords a
                          }

instance Monoid TokenizeSpec where
  mempty = TokenizeSpec { tsAtoms = mempty
                        , tsStringQQ = Nothing
                        , tsStringQ  = Nothing
                        , tsNoSlits = Nothing
                        , tsLineComment = mempty
                        , tsDelims = mempty
                        , tsStripLeft = Nothing
                        , tsStripRight = Nothing
                        , tsUW = Nothing
                        , tsNotNormalize = Nothing
                        , tsEsc = Nothing
                        , tsAddEmptyFields = Nothing
                        , tsPunct = mempty
                        , tsKeywords = mempty
                        }


justTrue :: Maybe Bool -> Bool
justTrue (Just True) = True
justTrue _ = False

-- | Turn on character escaping inside string literals.
-- Currently the following escaped characters are
-- supported: [" ' \ t n r a b f v ]
esc :: TokenizeSpec
esc = mempty { tsEsc = pure True }

-- | Raise empty field tokens (note mkEmpty method)
-- when no tokens found before a delimeter.
-- Useful for processing CSV-like data in
-- order to distingush empty columns
addEmptyFields :: TokenizeSpec
addEmptyFields = mempty { tsAddEmptyFields = pure True }

-- | same as addEmptyFields
emptyFields :: TokenizeSpec
emptyFields = addEmptyFields

-- | Turns off token normalization. Makes the tokenizer
-- generate character stream. Useful for debugging.
nn :: TokenizeSpec
nn = mempty { tsNotNormalize = pure True }

-- | Turns on single-quoted string literals.
-- Character stream after '\'' character
-- will be proceesed as single-quoted stream,
-- assuming all delimeter, comment and other special
-- characters as a part of the string literal until
-- the next unescaped single quote character.
sq :: TokenizeSpec
sq = mempty { tsStringQ = pure True }

-- | Enable double-quoted string literals support
-- as 'sq' for single-quoted strings.
sqq :: TokenizeSpec
sqq = mempty { tsStringQQ = pure True }

-- | Disable separate string literals.
--
-- Useful when processed delimeted data (csv-like formats).
-- Normally, sequential text chunks are concatenated together,
-- but consequent text and string literal will produce the two
-- different tokens and it may cause weird results if data
-- is in csv-like format, i.e:
--
-- >>> tokenize (delims ":"<>emptyFields<>sq ) "aaa:bebe:'qq' aaa:next::" :: [Maybe Text]
-- [Just "aaa",Just "bebe",Just "qq",Just " aaa",Just "next",Nothing,Nothing]
--
-- look: "qq" and " aaa" are turned into two separate tokens that makes the result
-- of CSV processing looks improper, like it has an extra-column. This behavior may be
-- avoided using this option, if you don't need to distinguish text chunks and string
-- literals:
--
-- >>> tokenize (delims ":"<>emptyFields<>sq<>noslits) "aaa:bebe:'qq:foo' aaa:next::" :: [Maybe Text]
-- [Just "aaa",Just "bebe",Just "qq:foo aaa",Just "next",Nothing,Nothing]
--
noslits :: TokenizeSpec
noslits = mempty { tsNoSlits = pure True }

-- | Specify the list of delimers (characters)
-- to split the character stream into fields.  Useful for CSV-like separated formats.  Support for
-- empty fields in token stream may be enabled by 'addEmptyFields' function
delims :: String -> TokenizeSpec
delims s = mempty { tsDelims = Set.fromList s }

-- | Strip spaces on left side of a token.
-- Does not affect string literals, i.e string are processed normally. Useful mostly for
-- processing CSV-like formats, otherwise 'delims' may be used to skip unwanted spaces.
sl :: TokenizeSpec
sl = mempty { tsStripLeft = pure True }

-- | Strip spaces on right side of a token.
-- Does not affect string literals, i.e string are processed normally. Useful mostly for
-- processing CSV-like formats, otherwise 'delims' may be used to skip unwanted spaces.
sr :: TokenizeSpec
sr = mempty { tsStripRight = pure True }

-- | Strips spaces on right and left sides and transforms multiple spaces into the one.
-- Name origins from  unwords . words
--
-- Does not affect string literals, i.e string are processed normally. Useful mostly for
-- processing CSV-like formats, otherwise 'delims' may be used to skip unwanted spaces.
uw :: TokenizeSpec
uw = mempty { tsUW = pure True }

-- | Specify the line comment prefix.
-- All text after the line comment prefix will
-- be ignored until the newline character appearance.
-- Multiple line comments are supported.
comment :: Text -> TokenizeSpec
comment s = mempty { tsLineComment = cmt }
  where
    cmt = case Text.uncons s of
            Just (p,su) -> Map.singleton p su
            Nothing     -> mempty

-- | Specify the punctuation characters.
-- Any punctuation character is handled as a separate
-- token.
-- Any token will be breaked on a punctiation character.
--
-- Useful for handling ... er... punctuaton, like
--
-- >> function(a,b)
--
-- or
--
-- >> (apply function 1 2 3)
--
--
-- >>> tokenize spec "(apply function 1 2 3)" :: [Text]
-- ["(","apply","function","1","2","3",")"]
--
punct :: Text -> TokenizeSpec
punct s = mempty { tsPunct = Set.fromList (Text.unpack s) }

-- | Specify the keywords list.
-- Each keyword will be threated as a separate token.
keywords :: [Text] -> TokenizeSpec
keywords s = mempty { tsKeywords = Set.fromList s }

newtype TokenizeM w a = TokenizeM (RWS TokenizeSpec w () a)
                        deriving( Applicative
                                , Functor
                                , MonadReader TokenizeSpec
                                , MonadWriter w
                                , MonadState  ()
                                , Monad
                                )

data Token = TChar Char
           | TSChar Char
           | TPunct Char
           | TText Text
           | TSLit Text
           | TKeyword Text
           | TEmpty
           | TDelim
           deriving (Eq,Ord,Show)

-- | Typeclass for token values.
-- Note, that some tokens appear in results
-- only when 'nn' option is set, i.e. sequences
-- of characters turn out to text tokens or string literals
-- and delimeter tokens are just removed from the
-- results
class IsToken a where
  -- |  Create a character token
  mkChar   :: Char -> a
  -- | Create a string literal character token
  mkSChar  :: Char -> a
  -- | Create a punctuation token
  mkPunct  :: Char -> a
  -- | Create a text chunk token
  mkText   :: Text -> a
  -- | Create a string literal token
  mkStrLit :: Text -> a
  -- | Create a keyword token
  mkKeyword :: Text -> a
  -- | Create an empty field token
  mkEmpty  :: a
  -- | Create a delimeter token
  mkDelim  :: a
  mkDelim = mkEmpty

instance IsToken (Maybe Text) where
  mkChar = pure . Text.singleton
  mkSChar = pure . Text.singleton
  mkPunct = pure . Text.singleton
  mkText = pure
  mkStrLit = pure
  mkKeyword = pure
  mkEmpty = Nothing

instance IsToken Text where
  mkChar   = Text.singleton
  mkSChar  = Text.singleton
  mkPunct  = Text.singleton
  mkText   = id
  mkStrLit = id
  mkKeyword = id
  mkEmpty  = ""

-- | Tokenize a text
tokenize :: IsToken a => TokenizeSpec -> Text -> [a]
tokenize s t = map tr t1
  where
    t1 = tokenize' s t
    tr (TChar c) = mkChar c
    tr (TSChar c) = mkSChar c
    tr (TText c) = mkText c
    tr (TSLit c) = mkStrLit c
    tr (TKeyword c) = mkKeyword c
    tr TEmpty  = mkEmpty
    tr (TPunct c) = mkPunct c
    tr TDelim  = mkDelim

execTokenizeM :: TokenizeM [Token] a -> TokenizeSpec -> [Token]
execTokenizeM (TokenizeM m) spec =
  let (_,w) = execRWS m spec () in norm w

  where norm x | justTrue (tsNotNormalize spec) = x
               | otherwise = normalize spec x

tokenize' :: TokenizeSpec -> Text -> [Token]
tokenize' spec txt = execTokenizeM (root txt) spec
  where

    root ts = do
      r <- ask

      case Text.uncons ts of
        Nothing           -> pure ()

        Just (c, rest)    | Set.member c (tsDelims r) -> tell [TDelim]  >> root rest
        Just ('\'', rest) | justTrue (tsStringQ r)    -> scanQ '\'' rest
        Just ('"', rest)  | justTrue (tsStringQQ r)   -> scanQ '"' rest

        Just (c, rest)    | Map.member c (tsLineComment r) -> scanComment (c,rest)

        Just (c, rest)    | Set.member c (tsPunct r)  -> tell [TPunct c] >> root rest

        Just (c, rest)    | otherwise                 -> tell [TChar c] >> root rest


    scanComment (c,rest) = do
      suff <- Map.lookup c <$> asks tsLineComment
      case suff of
        Just t | Text.isPrefixOf t rest -> do
           root $ Text.drop 1 $ Text.dropWhile ('\n' /=) rest

        _  -> tell [TChar c] >> root rest

    scanQ q ts = do
      r <- ask

      case Text.uncons ts of
        Nothing           -> root ts

        Just ('\\', rest) | justTrue (tsEsc r) -> unesc (scanQ q) rest
                          | otherwise          -> tell [tsChar '\\'] >> scanQ q rest

        Just (c, rest) | c ==  q   -> root rest
                       | otherwise -> tell [tsChar c] >> scanQ q rest

    unesc f ts =
      case Text.uncons ts of
        Nothing -> f ts
        Just ('"', rs)  -> tell [tsChar '"' ]  >> f rs
        Just ('\'', rs) -> tell [tsChar '\''] >> f rs
        Just ('\\', rs) -> tell [tsChar '\\'] >> f rs
        Just ('t', rs)  -> tell [tsChar '\t'] >> f rs
        Just ('n', rs)  -> tell [tsChar '\n'] >> f rs
        Just ('r', rs)  -> tell [tsChar '\r'] >> f rs
        Just ('a', rs)  -> tell [tsChar '\a'] >> f rs
        Just ('b', rs)  -> tell [tsChar '\b'] >> f rs
        Just ('f', rs)  -> tell [tsChar '\f'] >> f rs
        Just ('v', rs)  -> tell [tsChar '\v'] >> f rs
        Just (_, rs)    -> f rs

    tsChar c | justTrue (tsNoSlits spec) = TChar c
             | otherwise = TSChar c

newtype NormStats = NormStats { nstatBeforeDelim :: Int }

normalize :: TokenizeSpec -> [Token] -> [Token]
normalize spec tokens = snd $ execRWS (go tokens) () init
  where

    go [] = addEmptyField

    go (TChar c0 : cs) = do
      let (n,ns) = List.span isTChar cs
      succStat
      let chunk = eatSpaces $ Text.pack (c0 : [ c | TChar c <- n])
      let kw = Set.member chunk (tsKeywords spec)
      tell [ if kw then TKeyword chunk else TText chunk ]
      go ns

    go (TSChar x : xs) = do
      let (n,ns) = List.span isTSChar xs
      succStat
      tell [ TSLit $ Text.pack (x : [ c | TSChar c <- n]) ]
      go ns

    go (TDelim : xs) = do
      addEmptyField
      pruneStat
      go xs

    go (TPunct c : xs) = do
      tell [ TPunct c ]
      succStat
      go xs

    go (x:xs) = tell [x] >> go xs

    succStat = do
      modify (\x -> x { nstatBeforeDelim = succ (nstatBeforeDelim x)})

    pruneStat = do
      modify (\x -> x { nstatBeforeDelim = 0 } )

    addEmptyField = do
      ns <- gets nstatBeforeDelim
      when  (ns == 0 && justTrue (tsAddEmptyFields spec) ) $ do
        tell [ TEmpty ]

    isTChar (TChar _) = True
    isTChar _         = False

    isTSChar (TSChar _) = True
    isTSChar _          = False

    init = NormStats { nstatBeforeDelim = 0 }

    eatSpaces s | sboth  = Text.strip s
                | sLonly = Text.stripStart s
                | sRonly = Text.stripEnd s
                | sWU    = (Text.unwords . Text.words) s
                | otherwise = s

      where sboth  = justTrue (tsStripLeft spec) && justTrue (tsStripRight spec)
            sLonly = justTrue (tsStripLeft spec) && not (justTrue (tsStripRight spec))
            sRonly = not (justTrue (tsStripLeft spec)) && justTrue (tsStripRight spec)
            sWU    = justTrue (tsUW spec)

