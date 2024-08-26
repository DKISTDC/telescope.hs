module Telescope.Asdf.Encoding where

import Conduit
import Data.ByteString (ByteString)
import Data.Function ((&))
import Effectful
import Effectful.Error.Static
import Effectful.Resource
import Effectful.State.Static.Local
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.Encoding.File
import Telescope.Asdf.Encoding.Stream
import Telescope.Asdf.Error
import Text.Libyaml qualified as Yaml


encodeM :: (ToAsdf a, MonadIO m, MonadThrow m) => a -> m ByteString
encodeM a = runAsdfM $ encode a


encode :: (ToAsdf a, IOE :> es, Error AsdfError :> es) => a -> Eff es ByteString
encode a = do
  asdf <- toAsdfDoc a
  (tree, blks) <- encodeTreeBlocks asdf
  pure $ concatAsdfFile tree (encodeBlocks blks)


-- | Encode the tree and the data blocks
encodeTreeBlocks :: (IOE :> es, Error AsdfError :> es) => Asdf -> Eff es (EncodedTree, [BlockData])
encodeTreeBlocks a = do
  (doc, bds) <- runResource . runState @[BlockData] [] . runConduit $ yieldDocument a .| Yaml.encodeWith format
  pure (encodeTree doc, bds)
 where
  format = Yaml.defaultFormatOptions & Yaml.setTagRendering Yaml.renderUriTags
