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
  , keywords
  , HeaderFor (..)

    -- * Re-exports
  , LogicalConstant (..)
  , HeaderRecord (..)
  , KeywordRecord (..)
  ) where

import Telescope.Fits.Header.Class
import Telescope.Fits.Header.Header
import Telescope.Fits.Header.Keyword
import Telescope.Fits.Header.Value
import Prelude hiding (lookup)

