module Telescope.Fits.Header
  ( lookup
  , Header
  -- , Keyword
  , Value (..)
  -- , Comment
  ) where

import Data.Fits
import Prelude hiding (lookup)


-- addComment :: Comment -> Header -> Header
-- addComment c =
