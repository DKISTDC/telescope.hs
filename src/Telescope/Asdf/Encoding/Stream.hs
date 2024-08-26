module Telescope.Asdf.Encoding.Stream where

import Conduit
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
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
import Telescope.Asdf.Encoding.File (BlockData(..), BlockSource(..))
import Telescope.Asdf.Node
import Text.Libyaml (Event (..), MappingStyle (..), SequenceStyle (..), Style (..), Tag (..))
import Text.Libyaml qualified as Yaml

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
    yield $ EventMappingStart tag blockStyle Nothing
    mapM_ yieldMapping o
    yield EventMappingEnd
   where
    blockStyle
      | any (isComplexNode . snd) o = BlockMapping
      | otherwise = FlowMapping

  yieldMapping :: (Key, Node) -> ConduitT a Event (Eff es) ()
  yieldMapping (key, node) = do
    yield $ EventScalar (T.encodeUtf8 key) NoTag Plain Nothing
    yieldNode node

  yieldArray :: [Node] -> ConduitT a Event (Eff es) ()
  yieldArray ns = do
    yield $ EventSequenceStart tag seqStyle Nothing
    mapM_ yieldNode ns
    yield EventSequenceEnd
   where
    seqStyle
      | any isComplexNode ns = BlockSequence
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


isComplexNode :: Node -> Bool
isComplexNode (Node _ val) = isComplex val
 where
  isComplex = \case
    Array _ -> True
    Object _ -> True
    NDArray _ -> True
    _ -> False
