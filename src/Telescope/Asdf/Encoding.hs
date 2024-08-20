module Telescope.Asdf.Encoding where

import Conduit
import Data.ByteString (ByteString)
import Effectful
import Effectful.Error.Dynamic
import Effectful.Resource
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.Error
import Telescope.Asdf.Node
import Text.Libyaml (Event (..), Tag (..))
import Text.Libyaml qualified as Yaml


-- import Data.ByteString qualified as BS
-- import Data.ByteString.Lazy qualified as BL
-- import Telescope.Asdf.Document
-- import Telescope.Asdf.Node

-- but... it can't emit the parsing ones
-- encode :: (ToAsdf a, IOE :> es, Error AsdfError :> es) => a -> Eff es ByteString
-- encode a = do
--   -- TODO: binary
--   -- TODO: include the required comments, etc
--   -- TODO: fail if A doesn't serialize to an object
--   -- we are writing our own!
--   liftIO $ runEff $ encodeAsdf u

toDocument :: (ToAsdf a, Error AsdfError :> es) => a -> Eff es Asdf
toDocument a =
  case toValue a of
    Object o -> do
      let history = History []
      let library = telescopeSoftware
      pure $ Asdf{history, library, tree = o}
    value -> throwError $ EncodeError $ expected "Top-level Tree Object" value


encodeDocument :: (IOE :> es, Error AsdfError :> es) => Asdf -> Eff es ByteString
encodeDocument a = do
  runResource . runConduit $ yieldDocument a .| Yaml.encode


yieldDocument :: (Monad m) => Asdf -> ConduitT a Event m ()
yieldDocument a = do
  yield EventStreamStart
  yield EventDocumentStart
  yieldNode $ toNode a
  yield EventDocumentEnd
  yield EventStreamEnd


-- emit a bunch of events
yieldNode :: (Monad m) => Node -> ConduitT a Event m ()
yieldNode _ = do
  yield $ EventSequenceStart (UriTag "mytag") Yaml.BlockSequence Nothing
  yield $ EventScalar "woot" NoTag Yaml.Plain Nothing
  yield $ EventScalar "boot" NoTag Yaml.Plain Nothing
  yield $ EventScalar "woot" NoTag Yaml.Plain Nothing
  yield EventSequenceEnd


yieldValue :: (Monad m) => Value -> ConduitT a Event m ()
yieldValue _ = do
  yield $ EventSequenceStart (UriTag "mytag") Yaml.BlockSequence Nothing
  yield $ EventScalar "woot" NoTag Yaml.Plain Nothing
  yield $ EventScalar "boot" NoTag Yaml.Plain Nothing
  yield $ EventScalar "woot" NoTag Yaml.Plain Nothing
  yield EventSequenceEnd

-- testEncode :: IO ()
-- testEncode = do
--   (bs :: BS.ByteString) <- runEff . runResource . runConduit $ yieldNode undefined .| Yaml.encode
--   print $ BS.length bs
--   print bs
