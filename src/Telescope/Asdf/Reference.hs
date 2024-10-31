module Telescope.Asdf.Reference where

import Data.List ((!?))
import Data.Text (Text)
import Effectful
import Telescope.Asdf.NDArray
import Telescope.Asdf.Node
import Telescope.Data.Parser


-- resolveInternalPointer :: forall a es. (FromAsdf a, Reader Tree :> es, Parser :> es) => JSONPointer -> Eff es a
-- resolveInternalPointer point = do
--   Node _ value <- findPointer point
--   parseValue value

-- | Parse a 'JSONPointer' from a 'Tree'
findPointer :: forall es. (Parser :> es) => JSONPointer -> Tree -> Eff es Node
findPointer (JSONPointer path) (Tree tree) = do
  parseNext path (Node mempty Nothing (Object tree))
 where
  parseNext :: Path -> Node -> Eff es Node
  parseNext (Path []) node = pure node
  parseNext (Path (p : ps)) (Node _ _ val) = do
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
