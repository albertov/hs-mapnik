{-|
Module : Mapnik
Description : Public API of hs-mapnik
Copyright : (c) Alberto Valverde 2017
License : BSD3
Maintainer : alberto@toscat.net
Stability : experimental
Portability : POSIX

This module is the public API of the Haskell pure Mapnik model.
It is preferable to only import this module to use this package as
it will try to provide a stable API.  The internal modules are
still exported for convencience but depending on them can cause
breakage as Mapnik or these bindings evolve.

In order to achieve a stable API no constructors are exported from
this module. To construct values use the 'Default' instance's 'def'
implementation if provided or a smart constructor such as 'stop',
'rowLayout', 'pointSym', 'lineSym', etc.. and the lenses from
"Mapnik.Lens" to view/modify fields.

This module has no dependency on the Mapnik C++ library itself so it
can be used with 'ghcjs' or in native programs that don't want to link
against libmapnik. A bridge between these pure models and libmanik
is provided by the "Mapnik.Bindings" module in the 'hs-mapnik-bindings'
package
-}
module Mapnik (

-- * Main objects

-- ** Map
  Map

-- ** Layer
, Layer
, layer

-- ** Style
, Style
, StyleName
, Styles

-- ** Rule
, Rule

-- * Misc objects
--
-- ** Proj4
, Proj4
-- ** Box
--
, Box(Box)
-- ** Color
, Color (..)

-- ** Transform
, Transform

-- ** Expression
, Expression

-- ** Fonts
, Font
, FontSet
, FontSetMap
, FontSetName
, FaceName

-- ** FontFeatureSettings
, FontFeatureSettings

-- ** DashArray
, DashArray
, Dash (..)

-- * Symbolizers
, Symbolizer

-- ** PointSymbolizer
, pointSym

-- ** LineSymbolizer
, lineSym

-- ** LinePatternSymbolizer
, linePatternSym

-- ** PolygonSymbolizer
, polygonSym

-- ** PolygonPatternSymbolizer
, polygonPatternSym

-- ** RasterSymbolizer
, rasterSym
-- *** Colorizer
, Colorizer
, Stop
, stop

-- ** ShieldSymbolizer
, shieldSym

-- ** TextSymbolizer
, textSym
-- *** Properties
, TextSymProperties
, TextPlacements
, TextProperties
, TextFormatProperties
, TextLayoutProperties
-- **** Format
, Format
, formatExp
, formatList
, format_
, formatLayout

-- ** BuildingSymbolizer
, buildingSym

-- ** MarkersSymbolizer
, markersSym

-- ** GroupSymbolizer
, groupSym
-- *** Properties
, GroupSymProperties
-- **** GroupRule
, GroupRule
-- **** GroupLayout
, GroupLayout
, rowLayout
, pairLayout

-- ** DebugSymbolizer
, debugSym

-- ** DotSymbolizer
, dotSym

-- ** Symbolizer properties
, Prop (..)
, PropValue

-- * Enums
, module Mapnik.Enums

-- * Datasource
, Datasource (..)
, Parameter
, (.=)
, Parameters

-- * Value
, ToValue (..)
, Value (..)


-- * Lenses
, module Mapnik.Lens

-- * Re-exports
, Default(..)
) where

import Mapnik.Common hiding (minx,maxx,miny,maxy)
import Mapnik.Map
import Mapnik.Color
import Mapnik.Layer
import Mapnik.Style
import Mapnik.Rule
import Mapnik.Symbolizer
import Mapnik.Parameter
import Mapnik.Datasource
import Mapnik.Enums (
   CompositeMode(..)
 , AspectFixMode(..)
 , LineCap(..)
 , LineJoin(..)
 , LineRasterizer(..)
 , HaloRasterizer(..)
 , PointPlacement(..)
 , PatternAlignment(..)
 , DebugMode(..)
 , MarkerPlacement(..)
 , MarkerMultiPolicy(..)
 , TextTransform(..)
 , LabelPlacement(..)
 , VerticalAlignment(..)
 , HorizontalAlignment(..)
 , JustifyAlignment(..)
 , Upright(..)
 , Direction(..)
 , PlacementDirection(..)
 , GammaMethod(..)
 , SimplifyAlgorithm(..)
 , ScalingMethod(..)
 , ColorizerMode(..)
 )
import Mapnik.Lens

import Data.Default (Default(..))
