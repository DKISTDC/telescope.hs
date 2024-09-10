module Telescope.Asdf.Parser where

import Control.Monad.Catch (Exception, MonadCatch (..), MonadThrow)
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static


-- import Telescope.Asdf.Node

data ParseError
  = ParseFailure [Context] String
  deriving (Exception)


instance Show ParseError where
  show (ParseFailure ctx s) =
    "at " ++ intercalate "." (fmap show $ reverse ctx) ++ "\n ! " ++ s


data Context
  = Child Text


instance Show Context where
  show (Child c) = unpack c


-- the parser requires specific errors?
newtype Parser a = Parser
  { effect :: Eff '[Error ParseError, Reader [Context]] a
  }
  deriving newtype (Functor, Applicative, Monad, MonadThrow)


instance MonadCatch Parser where
  catch ma onErr = Parser $ do
    catch ma.effect (\e -> (onErr e).effect)


instance MonadFail Parser where
  fail s = Parser $ do
    ctx <- ask
    throwError $ ParseFailure ctx s


runParser :: Parser a -> Either String a
runParser p =
  case runPureEff . runErrorNoCallStack @ParseError $ fromParser p of
    Left pe -> Left $ show pe
    Right a -> Right a


fromParser :: (Error ParseError :> es) => Parser a -> Eff es a
fromParser p = do
  case runPureEff . runReader [] . runErrorNoCallStack @ParseError $ p.effect of
    Left e -> throwError e
    Right a -> pure a


addContext :: Context -> Parser a -> Parser a
addContext c p = Parser $ do
  local (c :) p.effect


context :: Parser [Context]
context = Parser ask
