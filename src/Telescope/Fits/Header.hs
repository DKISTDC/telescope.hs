module Telescope.Fits.Header
  ( Header (..)
  -- , Keyword
  , Value (..)
  -- , Comment
  , keyword
  , addComment

    -- * Re-exports
  , LogicalConstant (..)
  , getKeywords
  , HeaderRecord (..)
  , KeywordRecord (..)
  , ToHeader (..)
  , FromHeader (..)
  , ToKeyword (..)
  , FromKeyword (..)
  , parseKeyword
  , lookupKeyword
  , findKeyword
  , isKeyword
  , Parser
  , HeaderFor (..)
  ) where

import Data.Fits hiding (isKeyword, lookup)
import Data.Text (Text)
import Telescope.Data.Parser (Parser)
import Telescope.Fits.Header.Class
import Prelude hiding (lookup)


keyword :: Text -> Value -> Maybe Text -> HeaderRecord
keyword k v mc = Keyword $ KeywordRecord k v mc


addComment :: Text -> KeywordRecord -> KeywordRecord
addComment c kr = kr{_comment = Just c}
