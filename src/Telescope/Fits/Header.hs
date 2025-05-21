module Telescope.Fits.Header
  ( Header (..)
  , Value (..)
  , keyword
  , addComment

    -- * Parsing Headers
  , FromHeader (..)
  , FromKeyword (..)

    -- * Creating Headers
  , ToHeader (..)
  , ToKeyword (..)
  , parseKeyword

    -- * Keyword Lookup
  , lookupKeyword
  , findKeyword
  , isKeyword
  , HeaderFor (..)

    -- * Re-exports
  , LogicalConstant (..)
  , keywords
  , HeaderRecord (..)
  , KeywordRecord (..)
  ) where

import Data.Fits hiding (isKeyword, lookup)
import Data.Text (Text)
import Telescope.Fits.Header.Class
import Telescope.Fits.Header.Keyword (findKeyword, isKeyword, lookupKeyword)
import Prelude hiding (lookup)


-- | Construct a keyword HeaderRecord
keyword :: Text -> Value -> Maybe Text -> HeaderRecord
keyword k v mc = Keyword $ KeywordRecord k v mc


-- | Set the comment of a KeywordRecrod
addComment :: Text -> KeywordRecord -> KeywordRecord
addComment c kr = kr{comment = Just c}
