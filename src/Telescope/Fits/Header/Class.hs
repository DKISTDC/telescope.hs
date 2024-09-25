module Telescope.Fits.Header.Class where

import Data.Fits as Fits hiding (isKeyword)
import Data.List qualified as L
import Data.Text (Text, pack)
import Data.Text qualified as T
import Effectful
import GHC.Generics
import Telescope.Data.Parser
import Text.Casing (fromHumps, toSnake)


class ToKeyword a where
  toKeywordValue :: a -> Value


  -- Can ignore the selector name, modify it, etc
  toKeywordRecord :: Text -> a -> KeywordRecord
  default toKeywordRecord :: Text -> a -> KeywordRecord
  toKeywordRecord key a =
    KeywordRecord key (toKeywordValue a) Nothing


instance ToKeyword Int where
  toKeywordValue = Integer
instance FromKeyword Int where
  parseKeywordValue = \case
    Integer n -> pure n
    v -> expected "Integer" v


instance ToKeyword Float where
  toKeywordValue = Float
instance FromKeyword Float where
  parseKeywordValue = \case
    Float n -> pure n
    v -> expected "Float" v


instance ToKeyword Text where
  toKeywordValue = String
instance FromKeyword Text where
  parseKeywordValue = \case
    String n -> pure n
    v -> expected "String" v


instance ToKeyword Bool where
  toKeywordValue True = Logic T
  toKeywordValue False = Logic F
instance FromKeyword Bool where
  parseKeywordValue = \case
    Logic c -> pure $ c == T
    v -> expected "Logic" v


class FromKeyword a where
  parseKeywordValue :: (Parser :> es) => Value -> Eff es a


class ToHeader a where
  toHeader :: a -> Header
  default toHeader :: (Generic a, GToHeader (Rep a)) => a -> Header
  toHeader = gToHeader . from


instance (ToHeader a) => ToHeader (Maybe a) where
  toHeader Nothing = mempty
  toHeader (Just a) = toHeader a


instance (ToHeader a) => ToHeader [a] where
  toHeader = mconcat . fmap toHeader


class FromHeader a where
  parseHeader :: (Parser :> es) => Header -> Eff es a
  default parseHeader :: (Generic a, GFromHeader (Rep a), Parser :> es) => Header -> Eff es a
  parseHeader h = to <$> gParseHeader h


parseKeyword :: (FromKeyword a, Parser :> es) => Text -> Header -> Eff es a
parseKeyword k h =
  case Fits.lookup k h of
    Nothing -> parseFail $ "Missing key: " ++ show k
    Just v -> parseAt (Child k) $ parseKeywordValue v


class GToHeader f where
  gToHeader :: f p -> Header


instance (GToHeader f) => GToHeader (M1 D c f) where
  gToHeader (M1 f) = gToHeader f


instance (GToHeader f) => GToHeader (M1 C c f) where
  gToHeader (M1 f) = gToHeader f


instance (GToHeader f, GToHeader g) => GToHeader (f :*: g) where
  gToHeader (f :*: g) = gToHeader f <> gToHeader g


instance {-# OVERLAPPABLE #-} (ToKeyword a, Selector s) => GToHeader (M1 S s (K1 R a)) where
  gToHeader (M1 (K1 a)) = keywordForField (selName (undefined :: M1 S s f p)) a


instance {-# OVERLAPS #-} (ToKeyword a, Selector s) => GToHeader (M1 S s (K1 R (Maybe a))) where
  gToHeader (M1 (K1 Nothing)) = Header []
  gToHeader (M1 (K1 (Just a))) = keywordForField (selName (undefined :: M1 S s f p)) a


instance {-# OVERLAPS #-} (ToHeader a, Selector s) => GToHeader (M1 S s (K1 R (HeaderFor a))) where
  gToHeader (M1 (K1 (HeaderFor a))) = toHeader a


class GFromHeader f where
  gParseHeader :: (Parser :> es) => Header -> Eff es (f p)


instance (GFromHeader f) => GFromHeader (M1 D c f) where
  gParseHeader h = M1 <$> gParseHeader h


instance (GFromHeader f) => GFromHeader (M1 C c f) where
  gParseHeader h = M1 <$> gParseHeader h


instance (GFromHeader f, GFromHeader g) => GFromHeader (f :*: g) where
  gParseHeader h = do
    f <- gParseHeader h
    g <- gParseHeader h
    pure $ f :*: g


instance (FromKeyword a, Selector s) => GFromHeader (M1 S s (K1 R a)) where
  gParseHeader h = do
    let k = cleanKeyword $ selName (undefined :: M1 S s f p)
    M1 . K1 <$> parseKeyword k h


cleanKeyword :: String -> Text
cleanKeyword = T.toUpper . pack . toSnake . fromHumps


lookupKeyword :: Text -> Header -> Maybe Value
lookupKeyword k = findKeyword (isKeyword k)


findKeyword :: (KeywordRecord -> Bool) -> Header -> Maybe Value
findKeyword p h = do
  kr <- L.find p (getKeywords h)
  pure kr._value


isKeyword :: Text -> KeywordRecord -> Bool
isKeyword k (KeywordRecord k2 _ _) = T.toLower k == T.toLower k2


newtype HeaderFor a = HeaderFor a


keywordForField :: (ToKeyword a) => String -> a -> Header
keywordForField selector a =
  Header [Keyword $ toKeywordRecord (cleanKeyword selector) a]
