module Test.Asdf.TreeSpec where

import Skeletest
import Skeletest.Predicate qualified as P
import Skeletest.Prop.Gen qualified as Gen
import Skeletest.Prop.Range qualified as Range
import Telescope.Asdf.Document
import Telescope.Asdf.Tree


spec :: Spec
spec = do
  describe "basic" testBasic


-- describe "document" testSplitDocument

testBasic :: Spec
testBasic = do
  describe "UM" $ do
    it "alkjsdflkjkl" $ do
      fail "NOPE"

-- it "NOPE" $ do
--   2 `shouldBe` 4
--
