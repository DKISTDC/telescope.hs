module Telescope.Fits.HDU.Block where


{- | The size of an HDU block is fixed at thirty-six eighty byte words. In
    other words 2,880 bytes. These blocks are padded with zeros to this
    boundary.
-}
hduBlockSize :: Int
hduBlockSize = 2880


-- \| A single record in the HDU is an eighty byte word.
{-@ type HDURecordLength = {v:Int | v = 80} @-}
{-@ hduRecordLength :: HDURecordLength @-}
hduRecordLength :: Int
hduRecordLength = 80


-- \| The maximum amount of eighty byte records is thirty-six per the
--        standard.
--
{-@ type HDUMaxRecords = {v:Int | v = 36} @-}
{-@ hduMaxRecords :: HDUMaxRecords @-}
hduMaxRecords :: Int
hduMaxRecords = 36
