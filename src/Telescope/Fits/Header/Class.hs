module Telescope.Fits.Header.Class where

import Data.Fits as Fits hiding (isKeyword)
import Data.List qualified as L
import Data.Text (Text, pack)
import Data.Text qualified as T
import GHC.Generics
import Telescope.Data.Parser
import Text.Casing (fromHumps, toSnake)


class ToKeyword a where
  toKeywordValue :: a -> Value


instance ToKeyword Int where
  toKeywordValue = Integer
instance FromKeyword Int where
  parseKeywordValue = \case
    Integer n -> pure n
    v -> fail $ expected "Integer" v


instance ToKeyword Float where
  toKeywordValue = Float
instance FromKeyword Float where
  parseKeywordValue = \case
    Float n -> pure n
    v -> fail $ expected "Float" v


instance ToKeyword Text where
  toKeywordValue = String
instance FromKeyword Text where
  parseKeywordValue = \case
    String n -> pure n
    v -> fail $ expected "String" v


instance ToKeyword Bool where
  toKeywordValue True = Logic T
  toKeywordValue False = Logic F
instance FromKeyword Bool where
  parseKeywordValue = \case
    Logic c -> pure $ c == T
    v -> fail $ expected "Logic" v


class FromKeyword a where
  parseKeywordValue :: Value -> Parser a


class ToHeader a where
  toHeader :: a -> Header
  default toHeader :: (Generic a, GToHeader (Rep a)) => a -> Header
  toHeader = gToHeader . from


class FromHeader a where
  parseHeader :: Header -> Parser a
  default parseHeader :: (Generic a, GFromHeader (Rep a)) => Header -> Parser a
  parseHeader h = to <$> gParseHeader h


parseKeyword :: (FromKeyword a) => Text -> Header -> Parser a
parseKeyword k h =
  case Fits.lookup k h of
    Nothing -> fail $ "Missing key: " ++ show k
    Just v -> addContext (Child k) $ parseKeywordValue v


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
  gParseHeader :: Header -> Parser (f p)


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
  Header [Keyword $ KeywordRecord (cleanKeyword selector) (toKeywordValue a) Nothing]
