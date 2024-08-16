module Telescope.Asdf.Encoding where

import Conduit
import Data.ByteString (ByteString)
import Effectful
import Effectful.Error.Dynamic
import Effectful.Fail
import Effectful.Reader.Dynamic
import Effectful.Resource
import Telescope.Asdf.Document
import Telescope.Asdf.Node
import Telescope.Asdf.Tree
import Text.Libyaml (Event (..), Tag (..))
import Text.Libyaml qualified as Yaml


-- import Data.ByteString qualified as BS
-- import Data.ByteString.Lazy qualified as BL
-- import Telescope.Asdf.Document
-- import Telescope.Asdf.Node

-- encodeDocument :: Document -> BL.ByteString
-- encodeDocument n = _
--
--
-- decodeDocument :: BS.ByteString -> Document
-- decodeDocument inp = _

-- Decoding ------------------------------------

parseTree :: (Fail :> es, IOE :> es) => ByteString -> [BlockData] -> Eff es Node
parseTree inp blocks = do
  mn <- runErrorNoCallStack @YamlError . runResource . runReader blocks . runConduit $ Yaml.decode inp .| sinkDocument
  case mn of
    Left e -> fail $ "YAML " ++ show e
    Right n -> pure n
