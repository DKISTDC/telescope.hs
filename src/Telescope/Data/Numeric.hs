module Telescope.Data.Numeric where

import Data.Scientific (FPFormat (Generic), Scientific, formatScientific)
import Numeric (showFloat)


showFloat :: (RealFloat f) => f -> String
showFloat f = astropyExponent $ Numeric.showFloat f ""


showScientific :: Scientific -> String
showScientific = astropyExponent . formatScientific Generic Nothing


astropyExponent :: String -> String
astropyExponent s =
  case break (== 'e') s of
    (mantissa, 'e' : '-' : expo) -> mantissa <> "E-" <> expo
    (mantissa, 'e' : expo) -> mantissa <> "E+" <> expo
    _ -> s
