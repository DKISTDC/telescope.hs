{-# LANGUAGE UndecidableInstances #-}

module Telescope.Data.Parser where

import Control.Monad.Catch (Exception)
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Static


data Parser :: Effect where
  ParseFail :: String -> Parser m a
  PathMod :: (Path -> Path) -> m a -> Parser m a


type instance DispatchOf Parser = 'Dynamic


runParser
  :: Eff (Parser : es) a
  -> Eff es (Either ParseError a)
runParser = reinterpret (runErrorNoCallStack @ParseError . runReader @Path mempty) $ \env -> \case
  ParseFail e -> do
    path <- ask @Path
    throwError $ ParseFailure path e
  PathMod mp m -> do
    localSeqUnlift env $ \unlift -> local mp (unlift m)


-- copied from Effectful.Reader.Dynamic
-- localSeqUnlift env $ \unlift -> local (<> Path [p]) (unlift m)

data ParseError
  = ParseFailure Path String
  deriving (Exception, Eq)


instance Show ParseError where
  show (ParseFailure path s) =
    "at " ++ show path ++ "\n ! " ++ s


-- | Tracks the location of the parser in the document for error messages
newtype Path = Path [Ref]
  deriving (Eq)
  deriving newtype (Semigroup, Monoid)


instance Show Path where
  show (Path ps) =
    intercalate "/" (fmap show ps)


data Ref
  = Child Text
  | Index Int
  deriving (Eq)


instance Show Ref where
  show (Child c) = unpack c
  show (Index n) = show n


{- | Easy error message when we expect a particular type:

> instance FromKeyword Int where
>   parseKeywordValue = \case
>     Integer n -> pure n
>     v -> expected "Integer" v
-}
expected :: (Show value, Parser :> es) => String -> value -> Eff es a
expected ex n =
  parseFail $ "Expected " ++ ex ++ ", but got: " ++ show n


parseFail :: (Parser :> es) => String -> Eff es a
parseFail e = send $ ParseFail e


-- | Add a child to the parsing 'Path'
parseAt :: (Parser :> es) => Ref -> Eff es a -> Eff es a
parseAt p parse = do
  send $ PathMod (<> Path [p]) parse
