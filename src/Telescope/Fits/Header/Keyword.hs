module Telescope.Fits.Header.Keyword where

import Data.Text (Text)
import Data.Text qualified as T
import Telescope.Fits.HDU.Block (hduRecordLength)
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


keywordRecordLine :: KeywordRecord -> Text
keywordRecordLine (KeywordRecord k v mc) =
  T.justifyLeft 8 ' ' k
    <> "="
    <> T.justifyLeft (hduRecordLength - 10) ' ' (T.pack $ val v)
    <> inlineComment mc
 where
  val (Integer n) = show n
  val (Float f) = show f
  val (Logic l) = "              " ++ show l
  val (String t) = T.unpack t

  inlineComment Nothing = ""
  inlineComment (Just c) = " / " <> c


isKeyword :: Text -> KeywordRecord -> Bool
isKeyword k (KeywordRecord k2 _ _) = T.toLower k == T.toLower k2


-- | Set the comment of a KeywordRecrod
addComment :: Text -> KeywordRecord -> KeywordRecord
addComment c kr = kr{comment = Just c}
