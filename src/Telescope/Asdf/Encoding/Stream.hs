module Telescope.Asdf.Encoding.Stream where

import Conduit
import Data.ByteString (ByteString)
import Data.Conduit.Combinators (peek)
import Data.Conduit.Combinators qualified as C
import Data.List ((!?))
import Data.Text (pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.NonDet
import Effectful.Reader.Dynamic
import Effectful.State.Static.Local
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.Encoding.File
import Telescope.Asdf.NDArray (NDArrayData (..))
import Telescope.Asdf.Node
import Telescope.Asdf.Parser (ParseError, fromParser, runParser)
import Telescope.Data.Axes
import Text.Libyaml (Event (..), MappingStyle (..), SequenceStyle (..), Style (..), Tag (..))
import Text.Libyaml qualified as Yaml
import Text.Read (readMaybe)


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


-- Sink Decoding

sinkAsdf :: (Error YamlError :> es, Error ParseError :> es, Reader [BlockData] :> es) => ConduitT Yaml.Event o (Eff es) Asdf
sinkAsdf = do
  tree <- sinkTree
  lift $ fromParser (parseValue $ Object tree)


sinkTree :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Yaml.Event o (Eff es) Object
sinkTree = do
  expect EventStreamStart
  expect EventDocumentStart
  Node _ v <- sinkNode
  case v of
    Object o -> pure o
    _ -> lift $ throwError $ InvalidTree "Expected Object" v


sinkNode :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Yaml.Event o (Eff es) Node
sinkNode = do
  e <- event
  case e of
    EventScalar s t _ _ -> lift $ parseScalar s t
    EventMappingStart tg _ _ -> do
      let stag = parseSchemaTag tg
      if isNDArray stag
        then sinkNDArray stag
        else sinkObject stag
    EventSequenceStart tg _ _ -> do
      ns <- sinkSequence
      pure $ Node (parseSchemaTag tg) $ Array ns
    ev -> lift $ throwError $ ExpectedEvent "Not Handled" ev
 where
  sinkObject stag = do
    kvs <- sinkMappings
    pure $ Node stag $ Object kvs

  sinkNDArray stag = do
    dat <- sinkNDArrayData
    pure $ Node stag $ NDArray dat


sinkNDArrayData :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Event o (Eff es) NDArrayData
sinkNDArrayData = do
  kvs <- sinkMappings
  bytes <- require "source" kvs >>= findSource
  datatype <- require "datatype" kvs >>= parseDatatype
  byteorder <- require "byteorder" kvs >>= parseByteorder
  shape <- require "shape" kvs >>= parseShape
  pure $ NDArrayData{bytes, datatype, byteorder, shape}
 where
  require key kvs =
    case lookup key kvs of
      Nothing -> lift $ throwError $ NDArrayMissingKey (unpack key)
      Just (Node _ val) -> pure val

  parseDatatype val =
    case runParser $ parseValue val of
      Left _ -> lift $ throwError $ NDArrayExpected "DataType" val
      Right a -> pure a

  parseByteorder val =
    case runParser $ parseValue val of
      Left _ -> lift $ throwError $ NDArrayExpected "ByteOrder" val
      Right a -> pure a

  parseShape val =
    case val of
      Array ns -> axesRowMajor <$> mapM (parseAxis . (.value)) ns
      _ -> lift $ throwError $ NDArrayExpected "Shape" val

  parseAxis val =
    case val of
      Integer n -> pure $ fromIntegral n
      _ -> lift $ throwError $ NDArrayExpected "Shape Axis" val

  findSource val =
    case val of
      Integer s -> do
        blocks <- lift ask
        case blocks !? fromIntegral s of
          Nothing -> lift $ throwError $ NDArrayMissingBlock s
          Just (BlockData b) -> pure b
      _ -> lift $ throwError $ NDArrayExpected "Source" val


sinkMapping :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Event o (Eff es) (Key, Node)
sinkMapping = do
  k <- sinkMapKey
  v <- sinkNode
  pure (k, v)
 where
  sinkMapKey =
    event >>= \case
      EventScalar s _ _ _ -> pure $ decodeUtf8 s
      ev -> lift $ throwError $ ExpectedEvent "Scalar Key" ev


sinkMappings :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Event o (Eff es) [(Key, Node)]
sinkMappings = do
  sinkWhile (/= EventMappingEnd) sinkMapping


-- oh, the event mapping ends aren't being consumed!
sinkWhile :: (Event -> Bool) -> ConduitT Event o (Eff es) a -> ConduitT Event o (Eff es) [a]
sinkWhile p parse = do
  e <- peek
  if maybe False p e
    then do
      a <- parse
      as <- sinkWhile p parse
      pure (a : reverse as)
    else do
      -- consume the one we matched
      C.drop 1
      pure []


sinkSequence :: (Error YamlError :> es, Reader [BlockData] :> es) => ConduitT Event o (Eff es) [Node]
sinkSequence = do
  sinkWhile (/= EventSequenceEnd) sinkNode


parseScalar :: (Error YamlError :> es) => ByteString -> Yaml.Tag -> Eff es Node
parseScalar inp tg = byTag tg
 where
  byTag :: (Error YamlError :> es) => Yaml.Tag -> Eff es Node
  byTag = \case
    StrTag -> fromValue <$> parseStr inp -- always succeeds
    FloatTag -> throwEmpty "Float" $ fromValue <$> parseFloat inp
    IntTag -> throwEmpty "Int" $ fromValue <$> parseInt inp
    NullTag -> pure $ fromValue Null
    BoolTag -> throwEmpty "Bool" $ fromValue <$> parseBool inp
    UriTag s -> throwEmpty "Any" $ Node (schemaTag s) <$> parseMulti inp
    NoTag -> throwEmpty "Any" $ fromValue <$> parseMulti inp
    _ -> throwError $ InvalidScalarTag tg inp

  parseBool "true" = pure $ Bool True
  parseBool "false" = pure $ Bool False
  parseBool _ = empty

  parseStr s = pure $ String (decodeUtf8 s)

  parseInt s = Integer <$> parseRead s

  parseFloat s = Number <$> parseRead s

  parseMulti s =
    parseInt s <|> parseFloat s <|> parseBool s <|> parseStr s

  throwEmpty :: (Error YamlError :> es) => String -> Eff (NonDet : es) a -> Eff es a
  throwEmpty expt eff = do
    ec <- runNonDet OnEmptyKeep eff
    case ec of
      Left _ -> throwError $ InvalidScalar expt tg inp
      Right a -> pure a

  parseRead :: (Read a, NonDet :> es) => ByteString -> Eff es a
  parseRead s = do
    maybe empty pure $ readMaybe (unpack $ decodeUtf8 s)


-- | Await an event. Throw if out of input
event :: (Error YamlError :> es) => ConduitT i o (Eff es) i
event = do
  e <- await
  case e of
    Nothing -> lift $ throwError NoInput
    Just a -> pure a


parseSchemaTag :: Tag -> SchemaTag
parseSchemaTag (UriTag s) =
  let t = pack s
      mt = T.stripPrefix "tag:stsci.edu:asdf/" t
   in SchemaTag (maybe (pure t) pure mt)
parseSchemaTag _ = mempty


isNDArray :: SchemaTag -> Bool
isNDArray (SchemaTag Nothing) = False
isNDArray (SchemaTag (Just t)) =
  "core/ndarray" `T.isPrefixOf` t


expect :: (Error YamlError :> es) => Event -> ConduitT Event o (Eff es) ()
expect ex = expect' ("Exactly " ++ show ex) (== ex)


expect' :: (Error YamlError :> es) => String -> (Event -> Bool) -> ConduitT Event o (Eff es) ()
expect' ex p = do
  e <- event
  if p e
    then pure ()
    else lift $ throwError $ ExpectedEvent ex e


data YamlError
  = NoInput
  | ExpectedEvent String Event
  | InvalidScalar String Tag ByteString
  | InvalidScalarTag Tag ByteString
  | InvalidTree String Value
  | NDArrayMissingKey String
  | NDArrayMissingBlock Integer
  | NDArrayExpected String Value
  deriving (Show)


sinkIndex :: (Error YamlError :> es) => ConduitT Event o (Eff es) BlockIndex
sinkIndex = do
  expect EventStreamStart
  expect EventDocumentStart
  expect' "EventSequenceStart" isSequence
  ns <- sinkWhile (/= EventSequenceEnd) sinkIndexEntry
  expect EventSequenceEnd
  expect EventDocumentEnd
  expect EventStreamEnd
  pure $ BlockIndex ns
 where
  isSequence :: Event -> Bool
  isSequence EventSequenceStart{} = True
  isSequence _ = False

  sinkIndexEntry :: (Error YamlError :> es) => ConduitT Event o (Eff es) Int
  sinkIndexEntry = do
    e <- event
    case e of
      EventScalar s t _ _ -> do
        case readMaybe (unpack $ decodeUtf8 s) of
          Just n -> pure n
          Nothing -> lift $ throwError $ InvalidScalar "Int Index Entry" t s
      _ -> lift $ throwError $ ExpectedEvent "Scalar Int" e
