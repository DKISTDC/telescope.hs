module Telescope.Asdf.Parser where

import Control.Monad.Catch
import Effectful
import Effectful.Error.Static
import Effectful.Writer.Static.Local

-- import Telescope.Asdf.Class
import Telescope.Asdf.Node


data ParseError
  = ParseError String


data Context
  = Child Key


-- the parser requires specific errors?
newtype Parser a = Parser
  { parserEffect :: Eff '[Error ParseError, Writer [Context]] a
  }
  deriving newtype (Functor, Applicative, Monad)


instance MonadThrow Parser where
  throwM e = Parser $ throwError $ ParseError (show e)


instance MonadFail Parser where
  fail s = Parser $ throwError $ ParseError s


runParser :: Parser a -> Either String a
runParser p =
  case runPureEff . runWriter . runErrorNoCallStack $ p.parserEffect of
    (Left (ParseError e), ctx) -> Left $ formatError ctx e
    (Right a, _) -> Right a


-- TODO: include context
formatError :: [Context] -> String -> String
formatError _ s = s


parseNode :: Key -> Object -> Parser Node
parseNode k o =
  case lookup k o of
    Nothing -> fail $ "key " ++ show k ++ " not found"
    Just node -> do
      addContext $ Child k
      pure node


addContext :: Context -> Parser ()
addContext c = Parser $ tell [c]


expected :: (Show a) => String -> a -> String
expected ex n = "Expected " ++ ex ++ ", but got: " ++ show n
