module Data.Text.Fuzzy.Dates where

import Data.Attoparsec.Text (parseOnly)
import Data.Either (either)
import Data.Maybe
import Data.Text.Fuzzy.Attoparsec.Day
import Data.Text (Text)
import Data.Time.Calendar

parseMaybeDay :: Text -> Maybe Day
parseMaybeDay s = either (const Nothing) pure (parseOnly day s)

