module Telescope.Asdf.Encoding where

import Conduit
import Data.Binary.Put (runPut)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Function ((&))
import Data.Text (pack, unpack)
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.Resource
import Effectful.State.Static.Local
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.Error
import Telescope.Asdf.File
import Telescope.Asdf.Node
import Text.Libyaml (Event (..), MappingStyle (..), SequenceStyle (..), Style (..), Tag (..))
import Text.Libyaml qualified as Yaml


encodeM :: (ToAsdf a, MonadIO m) => a -> m ByteString
encodeM a =
  liftIO $ runEff . runErrorNoCallStackWith @AsdfError throwM $ encode a


encode :: (ToAsdf a, IOE :> es, Error AsdfError :> es) => a -> Eff es ByteString
encode a = do
  toDocument a >>= encodeDocument


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
  -- liftIO $ putStrLn "ENCODE DOCUMENT"
  (doc, blks) <- runResource . runState @[BlockData] [] . runConduit $ yieldDocument a .| Yaml.encodeWith format
  -- liftIO $ print headers
  -- liftIO $ print $ tree doc
  pure $
    BS.intercalate "\n" $
      headers <> tree doc <> blocks blks <> emptyIndex
 where
  headers = ["#ASDF 1.0.0", "#ASDF_STANDARD 1.5.0", "%YAML 1.1", tagDirective]
  tagDirective = "%TAG ! tag:stsci.edu:asdf/"
  tree doc = ["--- " <> doc, "..."]
  blocks blks = [BL.toStrict $ runPut $ putBlocks blks]
  -- TODO: generate a real index
  emptyIndex = ["%YAML 1.1", "---", "[]", "..."]
  format = Yaml.defaultFormatOptions & Yaml.setTagRendering Yaml.renderUriTags


yieldDocument :: (State [BlockData] :> es) => Asdf -> ConduitT a Event (Eff es) ()
yieldDocument a = do
  yield EventStreamStart
  yield EventDocumentStart
  yieldNode $ toNode a
  yield EventDocumentEnd
  yield EventStreamEnd


yieldNode :: forall es a. (State [BlockData] :> es) => Node -> ConduitT a Event (Eff es) ()
yieldNode (Node st val) = do
  case val of
    Object o -> yieldObject o
    Array a -> yieldArray a
    String s -> yieldScalar (T.encodeUtf8 s)
    Integer n -> yieldNum n
    NDArray nd -> yieldNDArray nd
    Bool b -> yieldBool b
    Number n -> yieldNum n
    Null -> yieldScalar "~"
 where
  tag = case st of
    SchemaTag Nothing -> NoTag
    SchemaTag (Just s) -> UriTag (unpack s)

  yieldScalar s = yield $ EventScalar s tag Plain Nothing

  yieldNum :: (Num n, Show n) => n -> ConduitT a Event (Eff es) ()
  yieldNum n = yieldScalar (T.encodeUtf8 $ pack $ show n)

  yieldBool = \case
    True -> yieldScalar "true"
    False -> yieldScalar "false"

  yieldObject :: [(Key, Node)] -> ConduitT a Event (Eff es) ()
  yieldObject o = do
    yield $ EventMappingStart tag BlockMapping Nothing
    mapM_ yieldMapping o
    yield EventMappingEnd

  yieldMapping :: (Key, Node) -> ConduitT a Event (Eff es) ()
  yieldMapping (key, node) = do
    yield $ EventScalar (T.encodeUtf8 key) NoTag Plain Nothing
    yieldNode node

  -- TODO: can we control whether they render inline or not? Depending on the length maybe?
  yieldArray :: [Node] -> ConduitT a Event (Eff es) ()
  yieldArray a = do
    yield $ EventSequenceStart tag seqStyle Nothing
    mapM_ yieldNode a
    yield EventSequenceEnd
   where
    seqStyle
      | length a > 10 = BlockSequence
      | otherwise = FlowSequence

  yieldNDArray :: NDArrayData -> ConduitT a Event (Eff es) ()
  yieldNDArray nd = do
    src <- lift $ addBlock nd.bytes
    yield $ EventMappingStart (UriTag "!core/ndarray-1.0.0") FlowMapping Nothing
    yieldMapping ("source", toNode src)
    yieldMapping ("datatype", toNode nd.datatype)
    yieldMapping ("shape", toNode nd.shape)
    yieldMapping ("byteorder", toNode nd.byteorder)
    yield EventMappingEnd


addBlock :: (State [BlockData] :> es) => ByteString -> Eff es BlockSource
addBlock bytes = do
  blocks <- get @[BlockData]
  put $ blocks <> [BlockData bytes]
  pure $ BlockSource $ length blocks
