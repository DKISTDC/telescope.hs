module Telescope.Fits.Header
  ( lookup
  , Header (..)
  -- , Keyword
  , Value (..)
  -- , Comment
  , keyword
  , addComment

    -- * Re-exports
  , toInt
  , toFloat
  , toText
  , LogicalConstant (..)
  , getKeywords
  , HeaderRecord (..)
  , KeywordRecord (..)
  , ToHeader (..)
  , FromHeader (..)
  , ToKeyword (..)
  , FromKeyword (..)
  , parseKeyword
  , Parser
  ) where

import Data.Fits hiding (isKeyword, lookup)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Telescope.Data.Parser (Parser)
import Telescope.Fits.Header.Class
import Prelude hiding (lookup)


keyword :: Text -> Value -> Maybe Text -> HeaderRecord
keyword k v mc = Keyword $ KeywordRecord k v mc


addComment :: Text -> KeywordRecord -> KeywordRecord
addComment c kr = kr{_comment = Just c}


lookup :: Text -> Header -> Maybe Value
lookup k h = do
  kr <- L.find (isKeyword k) (getKeywords h)
  pure kr._value


isKeyword :: Text -> KeywordRecord -> Bool
isKeyword k (KeywordRecord k2 _ _) = T.toLower k == T.toLower k2
