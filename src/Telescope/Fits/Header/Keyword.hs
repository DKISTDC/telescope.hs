module Telescope.Fits.Header.Keyword where

import Data.Fits as Fits hiding (isKeyword)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T


-- | Manually look up a keyword from the header
lookupKeyword :: Text -> Header -> Maybe Value
lookupKeyword k = findKeyword (isKeyword k)


findKeyword :: (KeywordRecord -> Bool) -> Header -> Maybe Value
findKeyword p h = do
  kr <- L.find p (keywords h)
  pure kr.value


isKeyword :: Text -> KeywordRecord -> Bool
isKeyword k (KeywordRecord k2 _ _) = T.toLower k == T.toLower k2
