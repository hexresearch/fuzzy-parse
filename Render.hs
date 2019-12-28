module Render where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Data
import GHC.Generics
import Control.Applicative ((<|>))

-- TODO: fixed width columns
-- TODO: column trim

data Entry a = Label Text | Value a
               deriving stock (Eq,Ord,Show,Data,Generic)

data RenderAlign = RenderAlignRight | RenderAlignLeft
                   deriving (Eq,Ord,Show)

data RenderSpec  = RenderSpec { rsAlign    :: Maybe RenderAlign
                              , rsMaxWidth :: Maybe Int
                              , rsLPad     :: Maybe Int
                              , rsRPad     :: Maybe Int
                              , rsLSep     :: Maybe Text
                              , rsRSep     :: Maybe Text
                              }
                   deriving (Eq,Ord,Show)


instance Semigroup RenderSpec where
  (<>) a b = RenderSpec { rsAlign    = rsAlign b    <|> rsAlign a
                        , rsMaxWidth = rsMaxWidth b <|> rsMaxWidth a
                        , rsLPad     = rsLPad b <|> rsRPad a
                        , rsRPad     = rsRPad b <|> rsRPad a
                        , rsLSep     = rsLSep b <|> rsLSep a
                        , rsRSep     = rsRSep b <|> rsRSep a
                        }

instance Monoid RenderSpec where
  mempty = RenderSpec { rsAlign    = Nothing
                      , rsMaxWidth = Nothing
                      , rsLPad     = Nothing
                      , rsRPad     = Nothing
                      , rsLSep     = Nothing
                      , rsRSep     = Nothing
                      }

lsep :: Text -> RenderSpec
lsep sep = mempty { rsLSep = pure sep }

rsep :: Text -> RenderSpec
rsep sep = mempty { rsRSep = pure sep }

rp :: Int -> RenderSpec
rp r = mempty { rsRPad = pure r }

lp :: Int -> RenderSpec
lp l = mempty { rsLPad = pure l }

alignNone  = mempty { rsAlign = Nothing }
alignLeft  = mempty { rsAlign = Just RenderAlignLeft  }
alignRight = mempty { rsAlign = Just RenderAlignRight }

maxWidth :: Int -> RenderSpec
maxWidth n = mempty { rsMaxWidth = pure n }

render :: Show a => RenderSpec -> Vector (Entry a) -> Vector (Entry a)
render spec vs = Vector.fromList [ Label (doAlign spec (l,s)) | (l,s) <- cells ]
  where

    fmt (Value a) = Text.pack $ show a
    fmt (Label t) = t

    cells  = [ (Text.length (fmt x), fmt x) | x <- Vector.toList vs ]
    maxlen = case (rsMaxWidth spec) of
             Just n  -> min n lmax
             Nothing -> lmax

      where lmax = maximum (fmap fst cells)

    doAlign _ (l,s) = lsep <> trunc (Text.replicate nl " " <> s <> Text.replicate nr " ") <> rsep
      where

        trunc s = case (rsMaxWidth spec) of
                    Nothing -> s
                    Just n  -> Text.take n s

        lsep = fromMaybe "" (rsLSep spec)
        rsep = fromMaybe "" (rsRSep spec)
        lp = fromMaybe 0 (rsLPad spec)
        rp = fromMaybe 0 (rsRPad spec)
        n  = max (maxlen - l) 0
        (nr,nl) = case (rsAlign spec) of
                    Just RenderAlignLeft  -> (rp+n,lp)
                    Just RenderAlignRight -> (rp,n+lp)
                    Nothing               -> (rp,n+lp)

