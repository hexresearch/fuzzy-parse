{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Text.Fuzzy.SExp where

import Data.Text (Text)

import Control.Applicative
import Data.Function
import Data.Functor
import Data.Text.Fuzzy.Tokenize
import Control.Monad.Reader
import Data.Map qualified as Map
import Data.Map (Map)
import Data.List qualified as List
import Data.Typeable
import Control.Monad.Except
import Control.Monad.RWS
import Data.Maybe
import Data.Char (isSpace)
import Data.Generics.Uniplate.Data()
import Safe
import Data.Data
import GHC.Generics
import Data.Generics.Uniplate.Operations
import Lens.Micro.Platform
import Data.Text qualified as Text
import Data.Coerce

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict  qualified as HM

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS

import UnliftIO

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
  | SyntaxError
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


instance MonadError SExpParseError m => MonadError SExpParseError (SExpM m) where
  throwError = lift . throwError
  catchError w  = catchError (coerce $ fromSexpM w)

tokenizeSexp :: Text -> [TTok]
tokenizeSexp txt =  do
  let spec = delims " \r\t\n" <> comment ";"
                          <> punct "'{}()[]"
                          <> sqq
                          <> uw
  tokenize spec txt

runSexpM :: Monad m => SExpM m a -> m a
runSexpM f = evalRWST (fromSexpM f) defEnv (SExpState 0 []) <&> fst


isBrace :: Char -> Bool
isBrace c = HM.member c braces

closing :: Char -> Maybe Char
closing c = HM.lookup c braces

isClosing :: Char -> Bool
isClosing = isJust . closing

braces :: HashMap Char Char
braces = HM.fromList[ ('{', '}')
                    , ('(', ')')
                    , ('[', ']')
                    , ('<', '>')
                    ]

oBraces :: [Char]
oBraces = HM.keys braces

cBraces :: [Char]
cBraces = HM.elems braces

parseSexp :: (MonadIO m, MonadError SExpParseError m) => Text -> m MicroSexp
parseSexp txt = do
  (s, _) <- runSexpM do
             (s,rest) <- sexp (tokenizeSexp txt)
             checkBraces
             pure (s,rest)

  pure s

checkBraces :: (MonadError SExpParseError m) => SExpM m ()
checkBraces = do
  braces <- gets (view sexpBraces)
  unless (null braces) $ lift $ throwError ParensUnder

parseTop :: MonadError SExpParseError m => Text -> m [MicroSexp]
parseTop txt = do
  let tokens = tokenizeSexp txt
  r <- S.toList_ $ runSexpM do
          flip fix (mempty,tokens) $ \next -> \case
            (acc, []) -> do
              emit acc
            -- (acc, TPunct '\n' : rest) -> do
            --   emit acc
            --   next (mempty,rest)
            (acc, rest) -> do
              (s, xs) <- sexp rest
              next (acc <> [s],xs)

  pure $ unlist (fmap unlist' r)

  where

    emit [] = pure ()
    emit x  = lift $ S.yield (List x)

    unlist' = \case
      List [List x] -> List x
      x -> x

    unlist = \case
      [List xs] -> xs
      other     -> other

sexp :: MonadError SExpParseError m => [TTok] -> SExpM m (MicroSexp, [TTok])
sexp s = case s of
  [] -> do
    checkBraces
    pure (nil, mempty)

  (TText s : w) -> (,w) <$> trNum (Symbol s)

  (TStrLit s : w) -> pure (String s, w)

  -- so far ignored
  (TPunct '\'' : rest) -> sexp rest

  (TPunct c : rest) | isSpace c  -> sexp rest

  (TPunct c : rest) | isBrace c  ->
    maybe (pure (nil, rest)) (`list` rest) (closing c)
                    | otherwise -> do
                        throwError ParensOver

  ( w : _ ) -> throwError SyntaxError

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
                    ( case  s of
                        "#t" -> Just (Boolean True)
                        "#f" -> Just (Boolean False)
                        _    -> Nothing
                    )

          pure $ fromMaybe (Symbol s) what


        x        -> pure x
    {-# INLINE trNum #-}

    list :: (MonadError SExpParseError m) => Char -> [TTok] -> SExpM m (MicroSexp, [TTok])

    list c [] = do
      throwError ParensUnder

    list c tokens = do
      modify $ over sexpBraces (c:)

      go c mempty tokens

      where

        isClosing :: Char -> Bool
        isClosing c = c `elem` cBraces

        go _ _ [] = do
          checkBraces
          pure (List mempty, mempty)

        go cl acc (TPunct c : rest)
          | isClosing c && c == cl = do
              modify $ over sexpBraces (drop 1)
              pure (List (reverse acc), rest)

          | isClosing c && c /= cl = do
              throwError ParensUnmatched

        go cl acc rest = do
          (e,r) <- sexp rest
          go cl (e : acc) r


