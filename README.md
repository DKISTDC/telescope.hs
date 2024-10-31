
Telescope
=========


[![Hackage](https://img.shields.io/hackage/v/telescope.svg&color=success)](https://hackage.haskell.org/package/telescope)

Haskell library to read and write Astronomical images from telescopes

* [FITS](https://fits.gsfc.nasa.gov/fits_standard.html) File Format
* [ASDF](https://asdf-standard.readthedocs.io/) File Format


Dependency on Fits-Parse
------------------------

Many thanks to @krakrjak for his hard work on [fits-parse](https://github.com/krakrjak/fits-parse)

Currently requires an unreleased commit. Add to your cabal.project

```
source-repository-package
  type: git
  location: https://github.com/krakrjak/fits-parse.git
  tag: 0823caab07b0f6574f3582759e085e70c282d821
```
