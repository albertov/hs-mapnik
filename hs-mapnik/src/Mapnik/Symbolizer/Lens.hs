{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Symbolizer.Lens where

import Mapnik.TH
import Mapnik.Symbolizer

import Control.Lens

type ApLens' s a = forall f . Applicative f => (a -> f a) -> s -> f s

class HasAllowOverlap s a | s -> a where allowOverlap :: ApLens' s a
class HasFill s a | s -> a where fill :: ApLens' s a
class HasAvoidEdges s a | s -> a where avoidEdges :: ApLens' s a
class HasDirection s a | s -> a where direction :: ApLens' s a
class HasDx s a | s -> a where dx :: ApLens' s a
class HasDy s a | s -> a where dy :: ApLens' s a

makeMapnikFields ''TextProperties
makeMapnikFields ''FormatProperties
makeMapnikFields ''TextLayoutProperties
makeMapnikFields ''Symbolizer
makePrisms ''Symbolizer
makePrisms ''Prop

{-
symbolizerLens :: Key a -> Lens' Symbolizer (PropValue a)
symbolizerLens k = properties . lens get_ set_ where
  get_ = fromMaybe PropDefault . DMap.lookup k
  set_ m PropDefault = DMap.delete k m
  set_ m v           = DMap.insert k v m
{-# INLINE symbolizerLens #-}

gamma :: Lens' Symbolizer (PropValue Double)
gamma = symbolizerLens Gamma

gammaMethod :: Lens' Symbolizer (PropValue GammaMethod)
gammaMethod = symbolizerLens GammaMethod

class HasOpacity o v | o->v where
  opacity :: Lens' o v

instance HasOpacity Symbolizer (PropValue Double) where
  opacity = symbolizerLens Opacity

alignment :: Lens' Symbolizer (PropValue PatternAlignment)
alignment = symbolizerLens Alignment

offset :: Lens' Symbolizer (PropValue Double)
offset = symbolizerLens Offset

compOp :: Lens' Symbolizer (PropValue CompositeMode)
compOp = symbolizerLens CompOp

clip :: Lens' Symbolizer (PropValue Bool)
clip = symbolizerLens Clip

instance HasFill Symbolizer (PropValue Color) where
  fill = symbolizerLens Fill

fillOpacity :: Lens' Symbolizer (PropValue Double)
fillOpacity = symbolizerLens FillOpacity

stroke :: Lens' Symbolizer (PropValue Color)
stroke = symbolizerLens Stroke

strokeWidth :: Lens' Symbolizer (PropValue Double)
strokeWidth = symbolizerLens StrokeWidth

strokeOpacity :: Lens' Symbolizer (PropValue Double)
strokeOpacity = symbolizerLens StrokeOpacity

strokeLinejoin :: Lens' Symbolizer (PropValue LineJoin)
strokeLinejoin = symbolizerLens StrokeLinejoin

strokeLinecap :: Lens' Symbolizer (PropValue LineCap)
strokeLinecap = symbolizerLens StrokeLinecap

strokeGamma :: Lens' Symbolizer (PropValue Double)
strokeGamma = symbolizerLens StrokeGamma

strokeGammaMethod :: Lens' Symbolizer (PropValue GammaMethod)
strokeGammaMethod = symbolizerLens StrokeGammaMethod

strokeDashoffset :: Lens' Symbolizer (PropValue Double)
strokeDashoffset = symbolizerLens StrokeDashoffset

strokeDasharray :: Lens' Symbolizer (PropValue DashArray)
strokeDasharray = symbolizerLens StrokeDasharray

strokeMiterlimit :: Lens' Symbolizer (PropValue Double)
strokeMiterlimit = symbolizerLens StrokeMiterlimit

geometryTransform :: Lens' Symbolizer (PropValue Transform)
geometryTransform = symbolizerLens GeometryTransform

lineRasterizer :: Lens' Symbolizer (PropValue LineRasterizer)
lineRasterizer = symbolizerLens LineRasterizer

imageTransform :: Lens' Symbolizer (PropValue Transform)
imageTransform = symbolizerLens ImageTransform

spacing :: Lens' Symbolizer (PropValue Double)
spacing = symbolizerLens Spacing

maxError :: Lens' Symbolizer (PropValue Double)
maxError = symbolizerLens MaxError

instance HasAllowOverlap Symbolizer (PropValue Bool)
  where allowOverlap = symbolizerLens AllowOverlap

ignorePlacement :: Lens' Symbolizer (PropValue Bool)
ignorePlacement = symbolizerLens IgnorePlacement

width :: Lens' Symbolizer (PropValue Double)
width = symbolizerLens Width

height :: Lens' Symbolizer (PropValue Double)
height = symbolizerLens Height

file :: Lens' Symbolizer (PropValue FilePath)
file = symbolizerLens File

shieldDx :: Lens' Symbolizer (PropValue Double)
shieldDx = symbolizerLens ShieldDx

shieldDy :: Lens' Symbolizer (PropValue Double)
shieldDy = symbolizerLens ShieldDy

unlockImage :: Lens' Symbolizer (PropValue Bool)
unlockImage = symbolizerLens UnlockImage

mode :: Lens' Symbolizer (PropValue Text)
mode = symbolizerLens Mode

scaling :: Lens' Symbolizer (PropValue Scaling)
scaling = symbolizerLens Scaling

filterFactor :: Lens' Symbolizer (PropValue Double)
filterFactor = symbolizerLens FilterFactor

meshSize :: Lens' Symbolizer (PropValue Int)
meshSize = symbolizerLens MeshSize

premultiplied :: Lens' Symbolizer (PropValue Bool)
premultiplied = symbolizerLens Premultiplied

smooth :: Lens' Symbolizer (PropValue Double)
smooth = symbolizerLens Smooth

simplifyAlgorithm :: Lens' Symbolizer (PropValue SimplifyAlgorithm)
simplifyAlgorithm = symbolizerLens SimplifyAlgorithm

simplifyTolerance :: Lens' Symbolizer (PropValue Double)
simplifyTolerance = symbolizerLens SimplifyTolerance

haloRasterizer :: Lens' Symbolizer (PropValue HaloRasterizer)
haloRasterizer = symbolizerLens HaloRasterizer

textPlacements :: Lens' Symbolizer (PropValue TextPlacements)
textPlacements = symbolizerLens TextPlacements

instance HasLabelPlacement Symbolizer (PropValue LabelPlacement) where
  labelPlacement = symbolizerLens LabelPlacement

markersPlacementType :: Lens' Symbolizer (PropValue MarkerPlacement)
markersPlacementType = symbolizerLens MarkersPlacementType

markersMultipolicy :: Lens' Symbolizer (PropValue MarkerMultiPolicy)
markersMultipolicy = symbolizerLens MarkersMultipolicy

pointPlacementType :: Lens' Symbolizer (PropValue PointPlacement)
pointPlacementType = symbolizerLens PointPlacementType

colorizer :: Lens' Symbolizer (PropValue Colorizer)
colorizer = symbolizerLens Colorizer

haloTransform :: Lens' Symbolizer (PropValue Transform)
haloTransform = symbolizerLens HaloTransform

numColumns :: Lens' Symbolizer (PropValue Int)
numColumns = symbolizerLens NumColumns

startColumn :: Lens' Symbolizer (PropValue Int)
startColumn = symbolizerLens StartColumn

repeatKey :: Lens' Symbolizer (PropValue Expression)
repeatKey = symbolizerLens RepeatKey

groupProperties :: Lens' Symbolizer (PropValue GroupProperties)
groupProperties = symbolizerLens GroupProperties

instance HasLargestBoxOnly Symbolizer (PropValue Bool) where
  largestBoxOnly = symbolizerLens LargestBoxOnly

instance HasMinimumPathLength Symbolizer (PropValue Double) where
  minimumPathLength = symbolizerLens MinimumPathLength

haloCompOp :: Lens' Symbolizer (PropValue CompositeMode)
haloCompOp = symbolizerLens HaloCompOp

instance HasTextTransform Symbolizer (PropValue TextTransform) where
  textTransform = symbolizerLens TextTransform

instance HasHorizontalAlignment Symbolizer (PropValue HorizontalAlignment) where
  horizontalAlignment = symbolizerLens HorizontalAlignment

instance HasJustifyAlignment Symbolizer (PropValue JustifyAlignment) where
  justifyAlignment = symbolizerLens JustifyAlignment

instance HasVerticalAlignment Symbolizer (PropValue VerticalAlignment) where
  verticalAlignment = symbolizerLens VerticalAlignment

instance HasUpright Symbolizer (PropValue Upright) where
  upright = symbolizerLens Upright

instance HasDirection Symbolizer (PropValue Direction) where
  direction = symbolizerLens Direction

instance HasAvoidEdges Symbolizer (PropValue Bool) where
  avoidEdges = symbolizerLens AvoidEdges

instance HasFfSettings Symbolizer (PropValue FontFeatureSettings) where
  ffSettings = symbolizerLens FfSettings
-}
