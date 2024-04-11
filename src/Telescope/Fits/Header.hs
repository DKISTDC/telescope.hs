module Telescope.Fits.Header
  ( lookup
  , Header (..)
  -- , Keyword
  , Value (..)
  -- , Comment
  , keyword
  , addComment
  ) where

import Data.Fits
import Data.Text (Text)
import Prelude hiding (lookup)


keyword :: Text -> Value -> Maybe Text -> HeaderRecord
keyword k v mc = Keyword $ KeywordRecord k v mc


addComment :: Text -> KeywordRecord -> KeywordRecord
addComment c kr = kr{_comment = Just c}

-- addComment :: Comment -> Header -> Header
-- addComment c =
