{-# LANGUAGE AllowAmbiguousTypes #-}

module Telescope.Asdf
  ( module Telescope.Asdf.Node
  , module Telescope.Asdf.Encoding
  , module Telescope.Asdf.Core
  )
where

import Telescope.Asdf.Core
import Telescope.Asdf.Encoding
import Telescope.Asdf.Node


-- - !core/column-1.0.0
--   data: !core/ndarray-1.0.0
--     source: 254
--     datatype: int64
--     byteorder: little
--     shape: [340]
--   name: VSPMAP

-- EXAMPLE ------------------------------------------------------
-- frame: !<tag:stsci.edu:gwcs/frame-1.0.0>
--   axes_names: [spatial along slit, dispersion axis, raster scan step number,
--     polarization state]
--   axes_order: [0, 1, 2, 3]
--   axes_type: [PIXEL, PIXEL, PIXEL, PIXEL]
--   axis_physical_types: ['custom:PIXEL', 'custom:PIXEL', 'custom:PIXEL', 'custom:PIXEL']
--   name: pixel
--   naxes: 4
--   unit: [!unit/unit-1.0.0 pixel, !unit/unit-1.0.0 pixel, !unit/unit-1.0.0 pixel,
--     !unit/unit-1.0.0 pixel]
--
--
-- EXAMPLE -------------------------------------------------------
-- - !unit/quantity-1.1.0 {unit: !unit/unit-1.0.0 pixel, value: 0.0}
--
--
--  points:
-- - !unit/quantity-1.1.0
--   unit: !unit/unit-1.0.0 pixel
--   value: !core/ndarray-1.0.0
--     source: 260
--     datatype: float64
--     byteorder: little
--     shape: [85]
