module Telescope.Fits.Header.Header where

import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Telescope.Fits.Header.Keyword
import Telescope.Fits.Header.Value


{- | The header part of the HDU is vital carrying not only authorship
    metadata, but also specifying how to make sense of the binary payload
    that starts 2,880 bytes after the start of the 'HeaderData'.
-}
newtype Header = Header {records :: [HeaderRecord]}
  deriving newtype (Eq, Semigroup, Monoid)


instance Show Header where
  show h =
    T.unpack $ T.intercalate "\n" $ fmap line h.records
   where
    line :: HeaderRecord -> Text
    line (Keyword kr) = keywordRecordLine kr
    line (History t) = "HISTORY " <> t
    line (Comment c) = "COMMENT " <> c
    line BlankLine = " "


{- | Headers contain lines that are any of the following

 > KEYWORD = VALUE / inline comment
 > COMMENT full line comment
 > (blank)
-}
data HeaderRecord
  = Keyword KeywordRecord
  | Comment Text
  | History Text
  | BlankLine
  deriving (Show, Eq)


-- | Manually look up a keyword from the header
lookupKeyword :: Text -> Header -> Maybe Value
lookupKeyword k = findKeyword (isKeyword k)


findKeyword :: (KeywordRecord -> Bool) -> Header -> Maybe Value
findKeyword p h = do
  kr <- L.find p (keywords h)
  pure kr.value


-- | Return all 'KeywordRecord's from the header, filtering out full-line comments and blanks
keywords :: Header -> [KeywordRecord]
keywords h = mapMaybe toKeyword h.records
 where
  toKeyword (Keyword k) = Just k
  toKeyword _ = Nothing


-- | Construct a keyword HeaderRecord
keyword :: Text -> Value -> Maybe Text -> HeaderRecord
keyword k v mc = Keyword $ KeywordRecord k v mc
