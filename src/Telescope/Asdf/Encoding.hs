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
  file <- encodeToAsdfFile asdf
  pure $ concatAsdfFile file


-- | Encode the tree and the data blocks
encodeToAsdfFile :: (IOE :> es, Error AsdfError :> es) => Asdf -> Eff es AsdfFile
encodeToAsdfFile a = do
  (doc, bds) <- runResource . runState @[BlockData] [] . runConduit $ yieldDocument a .| Yaml.encodeWith format
  let tree = encodeTree doc
  let blocks = fmap encodeBlock bds
  let index = encodeIndex $ blockIndex tree blocks
  pure $
    AsdfFile{tree, blocks, index}
 where
  format = Yaml.defaultFormatOptions & Yaml.setTagRendering Yaml.renderUriTags
