
Telescope
=========

[![Hackage Version](https://img.shields.io/hackage/v/telescope)](https://hackage.haskell.org/package/telescope)


Haskell library to read and write Astronomical images from telescopes

* [FITS](https://fits.gsfc.nasa.gov/fits_standard.html) (Flexible Image Transport System) Files
* [ASDF](https://asdf-standard.readthedocs.io/) (Advanced Scientific Data Format) Files

FITS Example
------------

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

We can use `Array` from [Data.Massiv](https://hackage.haskell.org/package/massiv) to parse, manipulate, and display data

```
  Image img : _ <- pure fits.extensions
  arr <- decodeDataArray @Ix2 @Float img.dataArray
  writeImage "hubble1.png" (heatmap arr)
```


![Hubble Output](https://raw.githubusercontent.com/DKISTDC/telescope.hs/main/example/output/hubble1.png)

#### Parsing Headers

Parse header keywords into Haskell records

```
import Telescope.Fits
import Effectful

data HubbleInfo = HubbleInfo
  { telescop :: Text
  , instrume :: Text
  , equinox :: Float
  } deriving (Generic, FromHeader, ToHeader)

main = do
  ...
  let h = fits.primaryHDU.header
  print $ lookupKeyword "INSTRUME" h

  case runPureEff $ runParser $ parseHeader h of
    Left e -> fail $ show e
    Right info -> do
      print info.telescop
      print info.equinox
```

ASDF Example
------------

ASDF files contain a YAML tree and binary data afterwards. They are readable and the tree is editable in a text editor.

```
example :: IO ()
example = do
  inp <- BS.readFile "../samples/dkist.asdf"
  dset :: Dataset <- decodeM inp
  print dset.info

data DKISTInfo = DKISTInfo
  { instrumentName :: Text
  , frameCount :: Int
  }
  deriving (Show, Generic, FromAsdf)


data Dataset = Dataset
  { info :: DKISTInfo
  }
instance FromAsdf Dataset where
  -- parse .dataset.meta.inventory into DKISTInfo
  parseValue val =
    case val of
      Object o -> do
        dset <- o .: "dataset"
        meta <- dset .: "meta"
        info <- meta .: "inventory"
        pure $ Dataset info
      k -> expected "Object" k
```


```
DKISTInfo {instrumentName = "VISP", frameCount = 1960}
```

Data can be parsed directly from the YAML tree or from binary-encoded data blocks (an NDArray)

```
data Example = Example
  { foo :: Int32
  , powers :: Maybe Powers
  , sequence :: [Int64]
  , random :: Array D Ix1 Double
  }
  deriving (Generic, FromAsdf)

example :: IO ()
example = do
  inp <- BS.readFile "../samples/example.asdf"
  ex :: Example <- decodeM inp
  print ex.name
  print ex.items
  print $ take 30 ex.sequence
  print $ take 10 $ M.toList ex.random
```

```
"Monty"
["one","two","three","four","five"]
[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29]
[0.7842310832387069,0.2279459557291822,0.9534462812074139,0.5100515929833191,0.6597920763222204,0.8778040169160855,0.8079416746447109,0.5373925949744411,0.5169365152585088,0.436101639340324]
```


### Collaborators

* The FITS code was developed in collaboration with @krakrjak
