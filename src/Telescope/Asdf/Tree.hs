module Telescope.Asdf.Tree where

import Conduit
import Control.Applicative (Alternative, (<|>))
import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Conduit.Combinators (peek)
import Data.Conduit.Combinators qualified as C
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Text.Lazy.IO qualified as TL
import Effectful
import Effectful.Error.Dynamic
import Effectful.Fail
import Effectful.NonDet
import Effectful.Resource
import GHC.IO (unsafePerformIO)
import Telescope.Asdf.Node
import Text.Libyaml (Event (..), Tag (..))
import Text.Libyaml qualified as Yaml
import Text.Read (readMaybe)


parseTree :: (Fail :> es, IOE :> es) => ByteString -> Eff es Node
parseTree inp = do
  mn <- runErrorNoCallStack @YamlError $ runResource $ runConduit $ Yaml.decode inp .| sinkDocument
  case mn of
    Left e -> fail $ "YAML " ++ show e
    Right n -> pure n


-- WARNING: IOE is required to get the MonadResource instance! Dumb
-- wild, they use unsafePerformIO
-- https://hackage.haskell.org/package/yaml-0.11.11.2/docs/src/Data.Yaml.html#decodeEither%27
-- parseTree :: (Fail :> es, IOE :> es) => ByteString -> Eff es Node
-- parseTree inp = do
--   -- TODO: run with unsafePerformIO somehow
--   runResource $ runConduit $ Yaml.decode inp .| sinkNode .| rootNode

-- runEff $ dumpTree inp
-- runEff $ testTree inp

sinkDocument :: (Error YamlError :> es) => ConduitT Yaml.Event o (Eff es) Node
sinkDocument = do
  expect EventStreamStart
  expect EventDocumentStart
  sinkNode


-- NOTE: START HERE
-- TODO: flesh these out
-- TODO: sinkDocument, should consume everything and produce a single document
sinkNode :: (Error YamlError :> es) => ConduitT Yaml.Event o (Eff es) Node
sinkNode = do
  -- handle the next one on the stack and gogogogogo
  e <- event
  case e of
    EventScalar s t _ _ -> lift $ parseScalar s t
    EventMappingStart tg _ _ -> do
      kvs <- sinkMappings
      pure $ Node (tag tg) $ Object kvs
    EventSequenceStart tg _ _ -> do
      ns <- sinkSequence
      pure $ Node (tag tg) $ Array ns
    ev -> lift $ throwError $ ExpectedEvent "Not Handled" ev


tag :: Tag -> SchemaTag
tag (UriTag s) =
  let t = pack s
      mt = T.stripPrefix "tag:stsci.edu:asdf/" t
   in SchemaTag (maybe (pure t) pure mt)
tag _ = mempty


expect :: (Error YamlError :> es) => Event -> ConduitT Event o (Eff es) ()
expect ex = do
  e <- event
  if e == ex
    then pure ()
    else lift $ throwError $ ExpectedEvent ("Exactly " ++ show ex) e


-- EventMappingStart NoTag BlockMapping Nothing
--  EventScalar "x" NoTag Plain Nothing
--  EventMappingStart (UriTag "tag:stsci.edu:asdf/unit/quantity-1.1.0") FlowMapping Nothing
--    EventScalar "unit" NoTag Plain Nothing
--    EventScalar "m" (UriTag "tag:stsci.edu:asdf/unit/unit-1.0.0") Plain Nothing
--    EventScalar "value" NoTag Plain Nothing
--    EventScalar "150322960057.57962" NoTag Plain Nothing
--  EventMappingEnd
--  EventScalar "y" NoTag Plain Nothing
--  EventMappingStart (UriTag "tag:stsci.edu:asdf/unit/quantity-1.1.0") FlowMapping Nothing
--    EventScalar "unit" NoTag Plain Nothing
--    EventScalar "m" (UriTag "tag:stsci.edu:asdf/unit/unit-1.0.0") Plain Nothing
--    EventScalar "value" NoTag Plain Nothing
--    EventScalar "3663346.057533264" NoTag Plain Nothing
--  EventMappingEnd
--    EventScalar "z" NoTag Plain Nothing
--    EventMappingStart (UriTag "tag:stsci.edu:asdf/unit/quantity-1.1.0") FlowMapping Nothing
--    EventScalar "unit" NoTag Plain Nothing
--    EventScalar "m" (UriTag "tag:stsci.edu:asdf/unit/unit-1.0.0") Plain Nothing
--    EventScalar "value" NoTag Plain Nothing
--    EventScalar "-10884763676.845081" NoTag Plain Nothing
--  EventMappingEnd
-- EventMappingEnd

sinkMapping :: (Error YamlError :> es) => ConduitT Event o (Eff es) (Key, Node)
sinkMapping = do
  k <- sinkMapKey
  v <- sinkNode
  pure (k, v)
 where
  sinkMapKey =
    event >>= \case
      EventScalar s _ _ _ -> pure $ decodeUtf8 s
      ev -> lift $ throwError $ ExpectedEvent "Scalar Key" ev


sinkMappings :: (Error YamlError :> es) => ConduitT Event o (Eff es) [(Key, Node)]
sinkMappings = do
  kvs <- sinkWhile (/= EventMappingEnd) sinkMapping
  pure kvs


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


sinkSequence :: (Error YamlError :> es) => ConduitT Event o (Eff es) [Node]
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
  throwEmpty expect eff = do
    ec <- runNonDet OnEmptyKeep eff
    case ec of
      Left _ -> throwError $ InvalidScalar expect tg inp
      Right a -> pure a

  parseRead :: (Read a, NonDet :> es) => ByteString -> Eff es a
  parseRead s = do
    maybe empty pure $ readMaybe (unpack $ decodeUtf8 s)


data YamlError
  = NoInput
  | ExpectedEvent String Yaml.Event
  | InvalidScalar String Tag ByteString
  | InvalidScalarTag Tag ByteString
  deriving (Exception, Show)


-- | Await an event. Throw if out of input
event :: (Error YamlError :> es) => ConduitT i o (Eff es) i
event = do
  e <- await
  case e of
    Nothing -> throwM NoInput
    Just a -> pure a

-- rootNode :: (Error YamlError :> es) => ConduitT Node Void (Eff es) Node
-- rootNode = do
--   mv <- C.head
--   case mv of
--     Nothing -> fail "Missing Root Value"
--     Just v -> pure v
