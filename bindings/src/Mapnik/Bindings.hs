{-|
Module : Mapnik.Bindings
Description : Public API of hs-mapnik-bindings
Copyright : (c) Alberto 2017
License : BSD3
Maintainer : alberto@toscat.net
Stability : experimental
Portability : POSIX

This module is the public API of the Haskell Mapnik bindings. The internal
modules are exported and can be used but their API might change unexpectedly
-}
module Mapnik.Bindings (
-- * Conversion between pure and impure structures
  FromMapnik (..)
, ToMapnik (..)

-- * Common
, Pair (..)

-- * Map
, fromXml
, fromXmlFile

-- * Datasource
, HsDatasource (..)
, Query (..)
, queryBox
, queryBoxProps
, features
, featuresAtPoint

-- * Feature
, Feature (..)
, feature

-- * Raster
, Raster
, RasterType
, mkRaster
, SomeRaster (..)
, getPixels

-- * Geometry
, Geometry
, toWkb
, toWkt
, fromWkb
, fromWkt

-- * ProjTransform
, ProjTransform
, projTransform
, CanTransform (..)

-- * Render
, RenderSettings (..)
, render
, renderSettings

-- * Image
, Image
, fromRgba8
, toRgba8
, serialize

-- * Registry
, registerDefaults
, registerPluginDir
, registerFontDir

-- * Lenses
, HasExtent (..)
, HasQueryExtent (..)
, HasNodata (..)
, HasVariables (..)
, HasScaleDenominator (..)
, HasScaleFactor (..)
, HasAspectFixMode (..)
, HasBox(..)
, HasUnBufferedBox(..)
, HasResolution(..)

-- * Exceptions
, MapnikError (..)

-- * Re-exports
, module Mapnik
) where

import Mapnik
import Mapnik.Bindings.Map
import Mapnik.Bindings.FromMapnik
import Mapnik.Bindings.ToMapnik
import Mapnik.Bindings.Image
import Mapnik.Bindings.Geometry
import Mapnik.Bindings.Types
import Mapnik.Bindings.Datasource
import Mapnik.Bindings.Feature
import Mapnik.Bindings.Raster
import Mapnik.Bindings.Render
import Mapnik.Bindings.Registry
import Mapnik.Bindings.Projection
