module Telescope.Asdf.Reference where

import Effectful
import Telescope.Asdf.Core (Asdf (..))
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Data.Parser


parsePointer :: Path -> Asdf -> Parser es Node
parsePointer = _


fetchExternal :: Reference -> Eff es Asdf
fetchExternal = _


-- wait, but we would also need the current tree
-- hmmm..... well....
-- parsing it directly huh?
-- so we have a tree... we can't convert it into
-- if it can hit a server, we need it to be an effect. We don't want that happening magically
-- What would the interface look like? We want to create a thing that
-- you'll have to parse it into a thing, then fetch it manually later... yeah...
-- not automatic
resolveReference :: Reference -> Eff es Asdf
resolveReference = _
