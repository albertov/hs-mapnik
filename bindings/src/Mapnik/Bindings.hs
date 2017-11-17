{-# LANGUAGE DuplicateRecordFields #-}
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
, Raster (..)
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
, Size
, PixelRgba8 (..)
, fromRgba8
, toRgba8
, serialize

-- * Registry
, registerDefaults
, registerPluginDir
, registerFontDir

-- * Re-exports
, CppException (..)
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
