module Test.Asdf.GWCSSpec where

import Data.List.NonEmpty qualified as NE
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.GWCS
import Telescope.Asdf.Node
import Telescope.Data.WCS
import Test.Asdf.ClassSpec (expectObject)


spec :: Spec
spec = do
  describe "toAsdf" toAsdfSpec
  describe "transforms" transformSpec


toAsdfSpec :: Spec
toAsdfSpec = do
  describe "Coorindate Frames" $ do
    it "should auto number axes order" $ do
      o <- expectObject $ toValue frame
      lookup "axes_order" o `shouldBe` Just (toNode @[Int] [0, 1])

    it "should order composite frame" $ do
      let frame2 = CoordinateFrame "boot" (NE.fromList [FrameAxis 2 "zero" "type" Pixel])
      let comp = CompositeFrame frame2 frame
      o <- expectObject $ toValue comp
      case lookup "frames" o of
        Just (Node _ _ (Array ns)) -> do
          case ns of
            [Node _ _ (Object f1), Node _ _ (Object f2)] -> do
              lookup "axes_order" f1 `shouldBe` Just (toNode @[Int] [2])
              lookup "axes_order" f2 `shouldBe` Just (toNode @[Int] [0, 1])
            _ -> fail $ "Expected frame objects" ++ show ns
        f -> fail $ "Expected frames" ++ show f
 where
  axes = [FrameAxis 0 "one" "type" Pixel, FrameAxis 1 "two" "type" Pixel]
  frame = CoordinateFrame "woot" (NE.fromList axes)


transformSpec :: Spec
transformSpec = do
  describe "linear" $ withMarkers ["focus"] $ do
    it "should calculate intercept as 0th value for crval = 0" $ do
      let wcs = WCSAxis{cunit = CUnit "cunit", ctype = CType "Ctype", crpix = 12.0, crval = 0, cdelt = 0.1}
      let Intercept int = wcsIntercept wcs
      int `shouldBe` (-1.1)

    it "should calculate intercept with crval" $ do
      let wcs = WCSAxis{cunit = CUnit "cunit", ctype = CType "Ctype", crpix = 12.0, crval = 1.0, cdelt = 0.1}
      let Intercept int = wcsIntercept wcs
      int `shouldSatisfy` P.approx P.tol (-0.1)
