{-# Language TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Data.Char
import Data.Data
import Data.Fixed
import Data.Generics.Uniplate.Data
import Data.Generics.Uniplate.Operations
import Data.Maybe
import Data.Text (Text)
import Data.Time.Calendar
import GHC.Generics
import Options.Generic
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Safe
import System.FilePath
import Text.InterpolatedString.Perl6 (qq,qc)


type Name = Text

type Roubles = Fixed E2

data CLI = Run FilePath
           deriving (Generic, Show)

instance ParseRecord CLI


data DateFact = DateDay Day
                deriving (Eq,Ord,Show,Data,Generic)

data ContragFact = ContragRaw Name
                   deriving (Eq,Ord,Show,Data,Generic)

data Fact =  Income  DateFact ContragFact Roubles
           | Expense DateFact ContragFact Roubles
            deriving (Eq,Ord,Show,Data,Generic)


-- TODO: распарсить
-- TODO: нормализовать названия
-- TODO: вывести разблюдовку по месяцам


getDate :: [Text] -> Maybe Day
getDate s = runIdentity $ do
  let toks = Text.splitOn "." (headDef "" s)
  case toks of
    [d,m,y] -> do let (di,mi,yi) = (readMay (u d), readMay (u m), readMay (u y))
                  pure $ fromGregorian <$>  (normY yi) <*> mi <*> di

    _       -> pure Nothing
  where u = Text.unpack
        normY x = x


getDayFact :: [Text]  -> [DateFact]
getDayFact s = maybeToList $ DateDay <$> getDate s

normNum :: Text -> Text
normNum s = [qc|{v2}.{d2}|]
  where
    (v,d) = Text.span (/='.') s
    v1 = Text.filter isDigit v
    d1 = Text.filter isDigit d
    v2 = case Text.length v1 of
           0 -> "0"
           _ -> v1

    d2 = case Text.length d1 of
           0 -> "00"
           1 -> [qc|{d1}0|]
           _ -> Text.take 2 d1

getNameFact :: [Text] -> [ContragFact]
getNameFact s = case s of
                  (_:x:_) -> [ContragRaw x]
                  _       -> mempty

getSummaText :: [Text] -> (Maybe Text, Maybe Text)
getSummaText s = case (Prelude.reverse s) of
                   (_:_:"":b:_) | not (Text.null b) ->  (pure $ normNum b, Nothing)
                   (_:_:a:"":_) | not (Text.null a) ->  (Nothing, pure $ normNum a)
                   (_:_:a:b:_)                      ->  (pure $ normNum b, pure $ normNum a)
                   _                                ->  (Nothing,Nothing)

getXferFact :: [Text] -> [Fact]
getXferFact ss =    mkFact Income undefined undefined ee
                 <> mkFact Expense undefined undefined ii
  where
    d1    = getDayFact ss
    n     = getNameFact ss
    (e,i) = getSummaText ss
    ee = Text.unpack <$> e >>= readMay
    ii = Text.unpack <$> i >>= readMay

    mkFact c (Just d) (Just n) (Just v) = [c d n v]
    mkFact _ _ _ _ = mempty

tokenizeTabs :: Text -> [Text]
tokenizeTabs s = map norm  $ Text.splitOn "\t" s
  where norm = Text.unwords . Text.words

main :: IO ()
main = do
  (Run fn)  <- getRecord "brunfuck"
  lns <- map tokenizeTabs . Text.lines <$> Text.readFile fn
  mapM_ (print <$> getDayFact) lns


