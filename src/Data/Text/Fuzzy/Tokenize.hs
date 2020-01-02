module Data.Text.Fuzzy.Tokenize ( TokenizeSpec(..)
                                , IsToken(..)
                                , tokenize
                                , esc
                                , addEmptyFields
                                , nn
                                , sq
                                , sqq
                                , delims
                                , comment
                                , punct
                                , keywords
                                ) where

import Data.Char
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.List as List
import Data.Monoid
import Control.Applicative

import Control.Monad.RWS

data TokenizeSpec = TokenizeSpec { tsAtoms          :: Set Text
                                 , tsStringQQ       :: Maybe Bool
                                 , tsStringQ        :: Maybe Bool
                                 , tsLineComment    :: Map Char Text
                                 , tsDelims         :: Set Char
                                 , tsEatSpace       :: Maybe Bool
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
                          , tsLineComment = tsLineComment b <> tsLineComment a
                          , tsDelims      = tsDelims b <> tsDelims a
                          , tsEatSpace    = tsEatSpace b <|> tsEatSpace a
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
                        , tsLineComment = mempty
                        , tsDelims = mempty
                        , tsEatSpace = Nothing
                        , tsNotNormalize = Nothing
                        , tsEsc = Nothing
                        , tsAddEmptyFields = Nothing
                        , tsPunct = mempty
                        , tsKeywords = mempty
                        }


justTrue :: Maybe Bool -> Bool
justTrue (Just True) = True
justTrue _ = False

esc :: TokenizeSpec
esc = mempty { tsEsc = pure True }

addEmptyFields = mempty { tsAddEmptyFields = pure True }

nn :: TokenizeSpec
nn = mempty { tsNotNormalize = pure True }

sq :: TokenizeSpec
sq = mempty { tsStringQ = pure True }

sqq :: TokenizeSpec
sqq = mempty { tsStringQQ = pure True }

delims :: String -> TokenizeSpec
delims s = mempty { tsDelims = Set.fromList s }

eatSpace :: TokenizeSpec
eatSpace = mempty { tsEatSpace = pure True }

comment :: Text -> TokenizeSpec
comment s = mempty { tsLineComment = cmt }
  where
    cmt = case Text.uncons s of
            Just (p,su) -> Map.singleton p su
            Nothing     -> mempty

punct :: Text -> TokenizeSpec
punct s = mempty { tsPunct = Set.fromList (Text.unpack s) }

keywords :: [Text] -> TokenizeSpec
keywords s = mempty { tsKeywords = Set.fromList s }

newtype TokenizeM w a = TokenizeM { untokenize :: RWS TokenizeSpec w () a }
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

class IsToken a where
  mkChar   :: Char -> a
  mkSChar  :: Char -> a
  mkPunct  :: Char -> a
  mkText   :: Text -> a
  mkStrLit :: Text -> a
  mkKeyword :: Text -> a
  mkEmpty  :: a
  mkDelim  :: a

instance IsToken (Maybe Text) where
  mkChar = pure . Text.singleton
  mkSChar = pure . Text.singleton
  mkPunct = pure . Text.singleton
  mkText = pure
  mkStrLit = pure
  mkKeyword = pure
  mkEmpty = Nothing
  mkDelim = Nothing

instance IsToken Text where
  mkChar   = Text.singleton
  mkSChar  = Text.singleton
  mkPunct  = Text.singleton
  mkText   = id
  mkStrLit = id
  mkKeyword = id
  mkEmpty  = ""
  mkDelim  = ""

tokenize :: IsToken a => TokenizeSpec -> Text -> [a]
tokenize s t = map tr t1
  where
    t1 = tokenize' s t
    tr (TChar c) = mkChar c
    tr (TSChar c) = mkSChar c
    tr (TText c) = mkText c
    tr (TSLit c) = mkStrLit c
    tr (TKeyword c) = mkKeyword c
    tr (TEmpty)  = mkEmpty
    tr (TPunct c) = mkPunct c
    tr (TDelim)  = mkDelim

execTokenizeM :: TokenizeM [Token] a -> TokenizeSpec -> [Token]
execTokenizeM (TokenizeM m) spec =
  let (_,w) = execRWS m spec () in (norm w)

  where norm x | justTrue (tsNotNormalize spec) = x
               | otherwise = normalize spec x

tokenize' :: TokenizeSpec -> Text -> [Token]
tokenize' spec txt = execTokenizeM (root txt) spec
  where

    root ts = do
      r <- ask

      case (Text.uncons ts) of
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
           root $ Text.drop 1 $ Text.dropWhile (\c -> c /= '\n') rest

        _  -> tell [TChar c] >> root rest

    scanQ q ts = do
      r <- ask

      case (Text.uncons ts) of
        Nothing           -> root ts

        Just ('\\', rest) | justTrue (tsEsc r) -> unesc (scanQ q) rest
                          | otherwise          -> tell [TSChar '\\'] >> scanQ q rest

        Just (c, rest) | c ==  q   -> root rest
                       | otherwise -> tell [TSChar c] >> scanQ q rest

    unesc f ts = do
      case (Text.uncons ts) of
        Nothing -> f ts
        Just ('"', rs)  -> tell [TSChar '"']  >> f rs
        Just ('\'', rs) -> tell [TSChar '\''] >> f rs
        Just ('\\', rs) -> tell [TSChar '\\'] >> f rs
        Just ('t', rs)  -> tell [TSChar '\t'] >> f rs
        Just ('n', rs)  -> tell [TSChar '\n'] >> f rs
        Just ('a', rs)  -> tell [TSChar '\a'] >> f rs
        Just ('b', rs)  -> tell [TSChar '\b'] >> f rs
        Just ('f', rs)  -> tell [TSChar '\f'] >> f rs
        Just ('v', rs)  -> tell [TSChar '\v'] >> f rs
        Just (_, rs)    -> f rs


data NormStats = NormStats { nstatBeforeDelim :: Int }

normalize :: TokenizeSpec -> [Token] -> [Token]
normalize spec x = snd $ execRWS (go x) () init
  where
    go (TChar x : xs) = do
      let (n,ns) = List.span isTChar xs
      succStat
      let chunk = Text.pack (x : [ c | TChar c <- n])
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

    go [] = addEmptyField

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

