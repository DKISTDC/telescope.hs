module Telescope.Asdf.Error where

import Control.Monad.Catch (Exception)


data AsdfError
  = YamlError String
  | BlockError String
  | ParseError String
  | EncodeError String
  deriving (Exception, Eq)


instance Show AsdfError where
  show (YamlError s) = "YamlError " ++ s
  show (BlockError s) = "BlockError " ++ s
  show (ParseError s) = "ParseError " ++ s
  show (EncodeError s) = "EncodeError " ++ s


expected :: (Show a) => String -> a -> String
expected ex n = "Expected " ++ ex ++ ", but got: " ++ show n
