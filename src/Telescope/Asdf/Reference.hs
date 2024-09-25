module Telescope.Asdf.Reference where

import Data.List ((!?))
import Data.Text (Text)
import Effectful
import Effectful.Reader.Dynamic
import Telescope.Asdf.Class
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Data.Parser


parsePointer :: forall a es. (FromAsdf a, Reader Tree :> es, Parser :> es) => Pointer -> Eff es a
parsePointer point = do
  Node _ value <- findPointer point
  parseValue value


findPointer :: forall es. (Reader Tree :> es, Parser :> es) => Pointer -> Eff es Node
findPointer (Pointer path) = do
  Tree tree <- ask
  parseNext path (Node mempty (Object tree))
 where
  parseNext :: Path -> Node -> Eff es Node
  parseNext (Path []) node = pure node
  parseNext (Path (p : ps)) (Node _ val) = do
    child <- parseSegment p val
    parseNext (Path ps) child

  parseSegment :: Ref -> Value -> Eff es Node
  parseSegment (Child n) (Object o) = parseChild n o
  parseSegment (Child n) node = missingPointer (Child n) node
  parseSegment (Index n) (Array a) = parseIndex n a
  parseSegment (Index n) node = missingPointer (Index n) node

  parseChild :: Text -> Object -> Eff es Node
  parseChild name o =
    case lookup name o of
      Nothing -> missingPointer (Child name) o
      Just c -> pure c

  parseIndex :: Int -> [Node] -> Eff es Node
  parseIndex n a =
    case a !? n of
      Nothing -> missingPointer (Index n) a
      Just c -> pure c

  missingPointer :: (Show ex, Show at) => ex -> at -> Eff es Node
  missingPointer expect at = parseFail $ "Could not locate pointer: " ++ show path ++ ". Expected " ++ show expect ++ " at " ++ show at

-- fetchExternal :: Reference -> Eff es Asdf
-- fetchExternal = _

-- resolveReference :: (Error ParseError :> es) => Reference -> Eff es Asdf
-- resolveReference = _
