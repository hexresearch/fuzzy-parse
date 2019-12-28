{-# Language TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE DerivingStrategies #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.Char
import Data.Data
import Data.Fixed
import Data.String(IsString(..))
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
import Text.Printf

import qualified Data.Matrix as M
import qualified Data.Vector as Vector
import Data.Vector (Vector)

import Render

newtype Name = Name Text
               deriving (Eq,Ord,Show,IsString,Data,Generic)

newtype Roubles = Roubles (Fixed E2)
                  deriving stock (Eq,Ord,Data,Generic)
                  deriving newtype (Num,Real,Read)

instance Show Roubles where
  show (Roubles x) = show x

data CLI = Run FilePath
         | Dump FilePath
         | MonthlyReport FilePath
         deriving (Generic, Show)

instance ParseRecord CLI


data DateFact = DateDay Day
              | DateMonth Int
              deriving (Eq,Ord,Show,Data,Generic)

data ContragForm = IP
                 | OOO
                 | PAO
                 | IFNS
                 | Other
                 deriving (Eq,Ord,Show,Data,Generic)

data ContragFact = ContragRaw Name
                 | Contrag ContragForm Name
                   deriving (Eq,Ord,Show,Data,Generic)

data XferType = Income | Expense
                deriving (Eq,Ord,Show,Data,Generic)

data Fact = Xfer XferType DateFact ContragFact Roubles
            deriving (Eq,Ord,Show,Data,Generic)

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
                  (_:x:_) -> [ContragRaw (Name x)]
                  _       -> mempty

getSummaText :: [Text] -> (Maybe Text, Maybe Text)
getSummaText s = case (Prelude.reverse s) of
                   (_:_:"":b:_) | not (Text.null b) ->  (pure $ normNum b, Nothing)
                   (_:_:a:"":_) | not (Text.null a) ->  (Nothing, pure $ normNum a)
                   (_:_:a:b:_)                      ->  (pure $ normNum b, pure $ normNum a)
                   _                                ->  (Nothing,Nothing)

getFacts :: [Text] -> [Fact]
getFacts ss =    mkFact Expense (listToMaybe d1) (listToMaybe n) ee
              <> mkFact Income (listToMaybe d1) (listToMaybe n) ii
  where
    d1    = getDayFact ss
    n     = getNameFact ss
    (e,i) = getSummaText ss
    ee = Text.unpack <$> e >>= readMay
    ii = Text.unpack <$> i >>= readMay

    mkFact c (Just d) (Just n) (Just v) = [Xfer c d n v]
    mkFact _ _ _ _ = mempty

tokenizeTabs :: Text -> [Text]
tokenizeTabs s = map norm  $ Text.splitOn "\t" s
  where norm = Text.unwords . Text.words

byYear :: Integer -> Fact -> Bool
byYear y f = and [ yOf d == y | (DateDay d) <- universeBi f ]
  where yOf d = let (y,_,_) = toGregorian d in y

toMonth (DateDay d) = DateMonth m
  where (y,m,_) = toGregorian d


getContragForm :: [Text] -> [ContragForm]
getContragForm = undefined

normNames (ContragRaw (Name nm)) = form
  where
    toks = map Text.toUpper $ Text.words nm
    form = case toks of
             ("ИП":xs)                               -> Contrag IP    (Name (shortNameIP xs))
             ("ИНДИВИДУАЛЬНЫЙ":"ПРЕДПРИНИМАТЕЛЬ":xs) -> Contrag IP    (Name (shortNameIP xs))
             xs          | isIFNS xs                 -> Contrag IFNS  (Name (shortFNS xs))
             xs          | isOOO xs                  -> Contrag OOO   (Name (shortOOO xs))
             xs                                      -> Contrag Other (Name (Text.unwords xs))


isIFNS :: [Text] -> Bool
isIFNS xs = not (Set.null (Set.fromList xs `Set.intersection` fns))
  where fns = Set.fromList ["ИНСПЕКЦИЯ", "ФЕДЕРАЛЬНОЙ", "НАЛОГОВОЙ", "СЛУЖБЫ"]

setOOO :: Set Text
setOOO = Set.fromList ["ОБЩЕСТВО", "ОГРАНИЧЕННОЙ", "ОТВЕТСТВЕННОСТЬЮ", "ООО"]

isOOO :: [Text] -> Bool
isOOO ("ООО":xs) = True
isOOO xs = not (Set.null (Set.fromList xs `Set.intersection` fns))
  where fns = setOOO

shortOOO :: [Text] -> Text
shortOOO xs = Text.unwords (dropC $ filter inOOO xs)
  where inOOO x = not $ x `Set.member` setOOO
        dropC ("С":xs) = xs
        dropC xs = xs


shortFNS xs = [qc|ИФНС {n}|]
  where n = headDef "" $ filter (Text.all isDigit) xs

shortNameIP :: [Text] -> Text
shortNameIP xs = case xs of
                  (f:n:p:_) -> [qc|{f} {s n}{s p}|]
                  _         -> Text.unwords xs
  where
    s :: Text -> Text
    s "" = ""
    s x  = [qc|{Text.head x}.|]


type MonthSumReport = Map ContragFact (Map Name Roubles)

runCmd :: CLI -> IO ()

runCmd (Dump fn) = do
  lns <- map tokenizeTabs . Text.lines <$> Text.readFile fn

  let facts'   = filter (byYear 2019) $ concatMap getFacts lns
  let facts''  = transformBi normNames $ transformBi toMonth  facts'

  let expenses = [e | e@(Xfer _ _ _ _) <- universeBi facts'']

  forM_ expenses $ \e -> do
    let ns = headDef "-" [ x | Name x       <- universeBi e]
    let m  = headDef 1   [ m | DateMonth m  <- universeBi e]
    let ru = headDef 0   [ r | Roubles r    <- universeBi e]

    printf "%02d %-32s %-8.2f\n"  m ns (realToFrac ru :: Double)

runCmd (Run fn) = do
  Text.putStrLn "deprecated"


runCmd (MonthlyReport fn) = do
  lns <- map tokenizeTabs . Text.lines <$> Text.readFile fn

  let facts'   = filter (byYear 2019) $ concatMap getFacts lns
  let facts''  = transformBi normNames $ transformBi toMonth  facts'

  let exp = [e | e@(Xfer Expense m c r) <- universeBi facts'']

  let m' = Map.fromListWith mkVal [ (c, Map.singleton m [r]) | e@(Xfer _ m c r) <- universeBi exp ]
  let m  = fmap (fmap sum) m'

  let mons = [1..12]

  let hdr = Vector.fromList $ Label "Name" : [Label (Text.pack $ show n) | n <- mons ]

  rows <- forM (Map.toList m) $ \(Contrag f (Name n),m2) -> do
            let cols = map (maybe (Label "") Value . flip Map.lookup m2) [DateMonth i | i <- mons]
            pure $ Vector.fromList $ (Label n : cols)

  let ma = M.fromRows (hdr:rows)

  let cols = case M.toColumns ma of
               (x:xs) -> render (alignLeft <> maxWidth 24) x : fmap (render (alignRight <> maxWidth 10 <> lsep "\t")) xs
               _      -> mempty

  mapM_ (drawNewline . Vector.imapM_ drawEntryM) (M.toRows (M.fromColumns cols))

  where
    mkVal :: Map DateFact [Roubles] -> Map DateFact [Roubles] -> Map DateFact [Roubles]
    mkVal = Map.unionWith (<>)



drawNewline m1 = m1 >> Text.putStrLn ""

drawEntryM :: Show a => Int -> Entry a -> IO ()
drawEntryM i cell = do

  let txt = case cell of
              Label txt -> txt
              Value a   -> error (show a)

  Text.putStr txt



main :: IO ()
main = do
  getRecord "brunfuck" >>= runCmd

