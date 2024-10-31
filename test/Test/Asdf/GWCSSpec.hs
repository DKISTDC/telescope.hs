module Test.Asdf.GWCSSpec where

import Data.List.NonEmpty qualified as NE
import GHC.Generics (Generic)
import Skeletest
import Skeletest.Predicate qualified as P
import Telescope.Asdf.Class
import Telescope.Asdf.Core
import Telescope.Asdf.GWCS
import Telescope.Asdf.Node
import Telescope.Data.WCS
import Test.Asdf.ClassSpec (expectObject)


data X deriving (Generic, ToAxes)
data Y deriving (Generic, ToAxes)
data Z deriving (Generic, ToAxes)


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
      let comp = CompositeFrame (frame2, frame)
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

  describe "(<&>) concatenate" $ withMarkers ["focus"] $ do
    it "should combine two inputs" $ do
      let tx = shift 10 :: Transform (Pix X) (Shift X)
      let ty = shift 20 :: Transform (Pix Y) (Shift Y)
      let total = tx <&> ty :: Transform (Pix X, Pix Y) (Shift X, Shift Y)
      total.transformation.forward `shouldSatisfy` P.con (Concat P.anything P.anything)

    it "should nest three inputs" $ do
      let tx = shift 10 :: Transform (Pix X) (Shift X)
      let ty = shift 20 :: Transform (Pix Y) (Shift Y)
      let tz = shift 20 :: Transform (Pix Z) (Shift Z)
      let total = tx <&> ty <&> tz :: Transform (Pix X, Pix Y, Pix Z) (Shift X, Shift Y, Shift Z)
      total.transformation.forward `shouldSatisfy` P.con (Concat P.anything P.anything)
      Concat txt tnext <- pure total.transformation.forward
      txt `shouldBe` tx.transformation

      tnext.forward `shouldSatisfy` P.con (Concat P.anything P.anything)
      Concat tyt tzt <- pure tnext.forward
      tyt `shouldBe` ty.transformation
      tzt `shouldBe` tz.transformation

  describe "concat and compose" $ withMarkers ["focus"] $ do
    it "should compose with higher priority first" $ do
      let tx = shift 10 :: Transform (Pix X) (Shift X)
      let tx2 = scale 10 :: Transform (Shift X) (Scale X)
      let ty = shift 20 :: Transform (Pix Y) (Shift Y)
      let ty2 = scale 10 :: Transform (Shift Y) (Scale Y)
      let total = tx |> tx2 <&> ty |> ty2 :: Transform (Pix X, Pix Y) (Scale X, Scale Y)

      total.transformation.forward `shouldSatisfy` P.con (Concat P.anything P.anything)
      Concat txt tyt <- pure total.transformation.forward

      txt.forward `shouldSatisfy` P.con (Compose P.anything P.anything)
      Compose txt1 txt2 <- pure txt.forward
      txt1 `shouldBe` tx.transformation
      txt2 `shouldBe` tx2.transformation

      tyt.forward `shouldSatisfy` P.con (Compose P.anything P.anything)
      Compose tyt1 tyt2 <- pure tyt.forward
      tyt1 `shouldBe` ty.transformation
      tyt2 `shouldBe` ty2.transformation
