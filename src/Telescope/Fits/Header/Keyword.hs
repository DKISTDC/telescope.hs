module Telescope.Fits.Header.Keyword where

import Data.Text (Text)
import Data.Text qualified as T
import Telescope.Fits.Header.Value


{- | A single 80 character header keyword line of the form: KEYWORD = VALUE / comment
    KEYWORD=VALUE
-}
data KeywordRecord = KeywordRecord
  { keyword :: Text
  , value :: Value
  , comment :: Maybe Text
  }
  deriving (Show, Eq)


isKeyword :: Text -> KeywordRecord -> Bool
isKeyword k (KeywordRecord k2 _ _) = T.toLower k == T.toLower k2


-- | Set the comment of a KeywordRecrod
addComment :: Text -> KeywordRecord -> KeywordRecord
addComment c kr = kr{comment = Just c}
