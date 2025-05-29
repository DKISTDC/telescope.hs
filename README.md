
Telescope
=========

[![Hackage](https://img.shields.io/hackage/v/telescope.svg&color=success)](https://hackage.haskell.org/package/telescope)

Haskell library to read and write Astronomical images from telescopes

* [FITS](https://fits.gsfc.nasa.gov/fits_standard.html) File Format
* [ASDF](https://asdf-standard.readthedocs.io/) File Format

FITS Example
-------

```
import Telescope.Fits
import Data.ByteString qualified as BS

main :: IO ()
main = do
  inp <- BS.readFile "hubble.fits"
  fits <- decode inp
```

FITS files start with a primary *Header Data Unit*

```
  print fits.primaryHDU
```

```
DataHDU
  Header: 490 records
  data: 0 bytes
  dimensions:
    format: Int8
    axes: []
```

HDUs may contain both data and a *header*

```
  print fits.primaryHDU.header
```

```
SIMPLE  =                    T / conforms to FITS standard
BITPIX  =                    8 / array data type
NAXIS   =                    0 / number of array dimensions
EXTEND  =                    T
DATE    = '2009-11-10'         / date this file was written (yyyy-mm-dd)
FILETYPE= 'SCI'                / type of data found in data file

TELESCOP= 'HST'                / telescope used to acquire data
INSTRUME= 'WFPC2'              / identifier for instrument used to acquire data
EQUINOX =               2000.0 / equinox of celestial coord. system
...
```

The primary HDU is followed by multiple extensions in a similar format

```
  mapM_ print fits.extensions
```

```
Image: DataHDU
  Header: 103 records
  data: 12960000 bytes
  dimensions:
    format: Float
    axes: [1800,1800]

Image: DataHDU
  Header: 124 records
  data: 12960000 bytes
  dimensions:
    format: Float
    axes: [1800,1800]

Image: DataHDU
  Header: 123 records
  data: 12960000 bytes
  dimensions:
    format: Int32
    axes: [1800,1800]
```

We can use Massiv's `Array` to parse, manipulate, and display data

```
  Image img : _ <- pure fits.extensions
  arr <- decodeDataArray @Ix2 @Float img.dataArray
  writeImage "hubble1.png" (heatmap arr)
```

![Hubble Output](./example/output/hubble.png)

### Collaborators

* The FITS code was developed in collaboration with @krakrjak
