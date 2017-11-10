{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Mapnik.Bindings.Symbolizer.Property where

import Mapnik.Enums
import Mapnik.Common
import Mapnik.Symbolizer

import Control.Lens
import Data.Maybe (catMaybes)
import Data.List (foldl')

#define GET_BASE_PROPS \
    fmap (SimplifyTolerance :=>) simplifyTolerance \
  , fmap (Smooth            :=>) smooth            \
  , fmap (Clip              :=>) clip \
  , fmap (CompOp            :=>) compOp \
  , fmap (GeometryTransform :=>) geometryTransform \
  , fmap (SimplifyAlgorithm :=>) simplifyAlgorithm

#define GET_STROKE_PROPS \
    fmap (StrokeGamma       :=>) strokeGamma \
  , fmap (StrokeGammaMethod :=>) strokeGammaMethod \
  , fmap (StrokeDasharray   :=>) strokeDashArray \
  , fmap (StrokeDashoffset  :=>) strokeDashOffset \
  , fmap (StrokeMiterlimit  :=>) strokeMiterLimit \
  , fmap (StrokeWidth       :=>) strokeWidth \
  , fmap (StrokeOpacity     :=>) strokeOpacity \
  , fmap (Stroke            :=>) stroke \
  , fmap (StrokeLinejoin    :=>) strokeLineJoin \
  , fmap (StrokeLinecap     :=>) strokeLineCap \
  , GET_BASE_PROPS

data Property where
  (:=>) :: Key v -> Prop v -> Property

type Properties = [Property]

data Key a where
    Gamma :: Key Double
    GammaMethod :: Key GammaMethod
    Opacity :: Key Double
    Alignment :: Key PatternAlignment
    Offset :: Key Double
    CompOp :: Key CompositeMode
    Clip :: Key Bool
    Fill :: Key Color
    FillOpacity :: Key Double
    Stroke :: Key Color
    StrokeWidth :: Key Double
    StrokeOpacity :: Key Double
    StrokeLinejoin :: Key LineJoin
    StrokeLinecap :: Key LineCap
    StrokeGamma :: Key Double
    StrokeGammaMethod :: Key GammaMethod
    StrokeDashoffset :: Key Double
    StrokeDasharray :: Key DashArray
    StrokeMiterlimit :: Key Double
    GeometryTransform :: Key Transform
    LineRasterizer :: Key LineRasterizer
    ImageTransform :: Key Transform
    Spacing :: Key Double
    MaxError :: Key Double
    AllowOverlap :: Key Bool
    IgnorePlacement :: Key Bool
    Width :: Key Double
    Height :: Key Double
    File :: Key FilePath
    ShieldDx :: Key Double
    ShieldDy :: Key Double
    UnlockImage :: Key Bool
    Mode :: Key (Either DebugMode RasterMode)
    Scaling :: Key Scaling
    FilterFactor :: Key Double
    MeshSize :: Key Int
    Premultiplied :: Key Bool
    Smooth :: Key Double
    SimplifyAlgorithm :: Key SimplifyAlgorithm
    SimplifyTolerance :: Key Double
    HaloRasterizer :: Key HaloRasterizer
    TextPlacements :: Key TextPlacements
    LabelPlacement :: Key LabelPlacement
    MarkersPlacementType :: Key MarkerPlacement
    MarkersMultipolicy :: Key MarkerMultiPolicy
    PointPlacementType :: Key PointPlacement
    Colorizer :: Key Colorizer
    HaloTransform :: Key Transform
    NumColumns :: Key Int
    StartColumn :: Key Int
    RepeatKey :: Key Expression
    GroupProperties :: Key GroupProperties
    LargestBoxOnly :: Key Bool
    MinimumPathLength :: Key Double
    HaloCompOp :: Key CompositeMode
    TextTransform :: Key TextTransform
    HorizontalAlignment :: Key HorizontalAlignment
    JustifyAlignment :: Key JustifyAlignment
    VerticalAlignment :: Key VerticalAlignment
    Upright :: Key Upright
    Direction :: Key Direction
    AvoidEdges :: Key Bool
    FfSettings :: Key FontFeatureSettings


class HasProperties s a | s -> a where
  properties :: Lens' s a

instance HasProperties Symbolizer Properties where
  properties = lens getProps setProps where
    setProps sym@Point{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (File               :=>v) = s { file = Just v }
      step !s (Opacity            :=>v) = s { opacity = Just v }
      step !s (AllowOverlap       :=>v) = s { allowOverlap = Just v }
      step !s (IgnorePlacement    :=>v) = s { ignorePlacement = Just v }
      step !s (PointPlacementType :=>v) = s { pointPlacement = Just v }
      step !s (ImageTransform     :=>v) = s { imageTransform = Just v }
      step !s p                         = stepBase s p

    setProps sym@Line{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (Opacity :=>v) = s { opacity = Just v }
      step !s (Offset  :=>v) = s { offset = Just v }
      step !s p              = stepStroke s p

    setProps sym@LinePattern{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (File            :=>v) = s { file = Just v }
      step !s (Opacity         :=>v) = s { opacity = Just v }
      step !s (Offset          :=>v) = s { offset = Just v }
      step !s (ImageTransform  :=>v) = s { imageTransform = Just v }
      step !s p                      = stepBase s p

    setProps sym@Polygon{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (Fill        :=>v) = s { fill = Just v }
      step !s (FillOpacity :=>v) = s { fillOpacity = Just v }
      step !s (Gamma       :=>v) = s { gamma = Just v }
      step !s (GammaMethod :=>v) = s { gammaMethod = Just v }
      step !s p                  = stepBase s p

    setProps sym@PolygonPattern{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (File           :=>v) = s { file = Just v }
      step !s (Opacity        :=>v) = s { opacity = Just v }
      step !s (Gamma          :=>v) = s { gamma = Just v }
      step !s (GammaMethod    :=>v) = s { gammaMethod = Just v }
      step !s (ImageTransform :=>v) = s { imageTransform = Just v }
      step !s (Alignment      :=>v) = s { alignment = Just v }
      step !s p                     = stepBase s p

    setProps sym@Raster{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (Mode          :=> Val (Right v)) = s { rasterMode = Just v }
      step !s (Scaling       :=> Val v) = s { scaling = Just v }
      step !s (Opacity       :=> Val v) = s { rasterOpacity = Just v }
      step !s (FilterFactor  :=> Val v) = s { filterFactor = Just v }
      step !s (MeshSize      :=> Val v) = s { meshSize = Just v }
      step !s (Premultiplied :=> Val v) = s { preMultiplied = Just v }
      step !s (Colorizer     :=> Val v) = s { colorizer = Just v }
      step !s p                         = stepBase s p

    setProps sym@Shield{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (TextPlacements :=>v) = s { placements = Just v }
      step !s (ImageTransform :=>v) = s { imageTransform = Just v }
      step !s (ShieldDx       :=>v) = s { dx = Just v }
      step !s (ShieldDy       :=>v) = s { dy = Just v }
      step !s (Opacity        :=>v) = s { opacity = Just v }
      step !s (UnlockImage    :=>v) = s { unlockImage = Just v }
      step !s (File           :=>v) = s { file = Just v }
      step !s (HaloRasterizer :=>v) = s { haloRasterizer = Just v }
      step !s p                     = stepBase s p

    setProps sym@Text{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (TextPlacements :=>v) = s { placements = Just v }
      step !s (HaloCompOp     :=>v) = s { haloCompOp = Just v }
      step !s (HaloRasterizer :=>v) = s { haloRasterizer = Just v }
      step !s (HaloTransform  :=>v) = s { haloTransform = Just v }
      step !s p                     = stepBase s p

    setProps sym@Building{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (Fill        :=>v) = s { fill = Just v }
      step !s (FillOpacity :=>v) = s { fillOpacity = Just v }
      step !s (Height      :=>v) = s { height = Just v }
      step !s p                  = stepBase s p

    setProps sym@Markers{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (File                  :=>v) = s { file = Just v }
      step !s (Opacity               :=>v) = s { opacity = Just v }
      step !s (Fill                  :=>v) = s { fill = Just v }
      step !s (FillOpacity           :=>v) = s { fillOpacity = Just v }
      step !s (Spacing               :=>v) = s { spacing = Just v }
      step !s (MaxError              :=>v) = s { maxError = Just v }
      step !s (Offset                :=>v) = s { offset = Just v }
      step !s (Width                 :=>v) = s { width = Just v }
      step !s (Height                :=>v) = s { height = Just v }
      step !s (AllowOverlap          :=>v) = s { allowOverlap = Just v }
      step !s (AvoidEdges            :=>v) = s { avoidEdges = Just v }
      step !s (IgnorePlacement       :=>v) = s { ignorePlacement = Just v }
      step !s (ImageTransform        :=>v) = s { imageTransform = Just v }
      step !s (MarkersPlacementType  :=>v) = s { placement = Just v }
      step !s (MarkersMultipolicy    :=>v) = s { multiPolicy = Just v }
      step !s (Direction             :=>v) = s { direction = Just v }
      step !s p                            = stepStroke s p

    setProps sym@Group{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (GroupProperties       :=>v) = s { groupProperties = Just v }
      step !s (NumColumns            :=>v) = s { numColumns = Just v }
      step !s (StartColumn           :=>v) = s { startColumn = Just v }
      step !s (RepeatKey             :=>v) = s { repeatKey = Just v }
      step !s (TextPlacements        :=>v) = s { placements = Just v }
      step !s p                            = stepBase s p

    setProps sym@Debug{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (Mode :=> Val (Left v)) = s { debugMode = Just v }
      step !s p                       = stepBase s p

    setProps sym@Dot{} = foldl' step sym  where
      step :: Symbolizer -> Property -> Symbolizer
      step !s (Fill    :=>v) = s { fill = Just v }
      step !s (Opacity :=>v) = s { opacity = Just v }
      step !s (Width   :=>v) = s { width = Just v }
      step !s (Height  :=>v) = s { height = Just v }
      step !s (CompOp  :=>v) = s { compOp = Just v }
      step !s _         = s

    getProps sym = catMaybes $ case sym of
      Point{..} ->
        [ fmap (File               :=>) file
        , fmap (Opacity            :=>) opacity
        , fmap (AllowOverlap       :=>) allowOverlap
        , fmap (IgnorePlacement    :=>) ignorePlacement
        , fmap (PointPlacementType :=>) pointPlacement
        , fmap (ImageTransform     :=>) imageTransform
        , GET_BASE_PROPS
        ]
      Line{..} ->
        [ fmap (Offset         :=>) offset
        , fmap (LineRasterizer :=>) lineRasterizer
        , GET_STROKE_PROPS
        ]
      LinePattern{..} ->
        [ fmap (File           :=>) file
        , fmap (Opacity        :=>) opacity
        , fmap (Offset         :=>) offset
        , fmap (ImageTransform :=>) imageTransform
        , GET_BASE_PROPS
        ]
      Polygon{..} ->
        [ fmap (Fill           :=>) fill
        , fmap (FillOpacity    :=>) fillOpacity
        , fmap (Gamma          :=>) gamma
        , fmap (GammaMethod    :=>) gammaMethod
        , GET_BASE_PROPS
        ]
      PolygonPattern{..} ->
        [ fmap (File           :=>) file
        , fmap (Opacity        :=>) opacity
        , fmap (Gamma          :=>) gamma
        , fmap (GammaMethod    :=>) gammaMethod
        , fmap (ImageTransform :=>) imageTransform
        , fmap (Alignment      :=>) alignment
        , GET_BASE_PROPS
        ]
      Raster{..} ->
        [ fmap ((Mode :=>) . Val . Right)  rasterMode
        , fmap ((Scaling       :=>) . Val) scaling
        , fmap ((Opacity       :=>) . Val) rasterOpacity
        , fmap ((FilterFactor  :=>) . Val) filterFactor
        , fmap ((MeshSize      :=>) . Val) meshSize
        , fmap ((Premultiplied :=>) . Val) preMultiplied
        , fmap ((Colorizer     :=>) . Val) colorizer
        , GET_BASE_PROPS
        ]
      Shield{..} ->
        [ fmap (TextPlacements    :=>) placements
        , fmap (GeometryTransform :=>) imageTransform
        , fmap (ShieldDx          :=>) dx
        , fmap (ShieldDy          :=>) dy
        , fmap (Opacity           :=>) opacity
        , fmap (UnlockImage       :=>) unlockImage
        , fmap (File              :=>) file
        , fmap (HaloRasterizer    :=>) haloRasterizer
        , GET_BASE_PROPS
        ]
      Text{..} ->
        [ fmap (TextPlacements    :=>) placements
        , fmap (HaloCompOp        :=>) haloCompOp
        , fmap (HaloRasterizer    :=>) haloRasterizer
        , fmap (GeometryTransform :=>) haloTransform
        , GET_BASE_PROPS
        ]
      Building{..} ->
        [ fmap (Fill              :=>) fill
        , fmap (FillOpacity       :=>) fillOpacity
        , fmap (Height            :=>) height
        , GET_BASE_PROPS
        ]
      Markers{..} ->
        [ fmap (File                 :=>) file
        , fmap (Opacity              :=>) opacity
        , fmap (Fill                 :=>) fill
        , fmap (FillOpacity          :=>) fillOpacity
        , fmap (Spacing              :=>) spacing
        , fmap (MaxError             :=>) maxError
        , fmap (Offset               :=>) offset
        , fmap (Width                :=>) width
        , fmap (Height               :=>) height
        , fmap (AllowOverlap         :=>) allowOverlap
        , fmap (AvoidEdges           :=>) avoidEdges
        , fmap (IgnorePlacement      :=>) ignorePlacement
        , fmap (GeometryTransform    :=>) imageTransform
        , fmap (MarkersPlacementType :=>) placement
        , fmap (MarkersMultipolicy   :=>) multiPolicy
        , fmap (Direction            :=>) direction
        , GET_STROKE_PROPS
        ]
      Group{..} ->
        [ fmap (GroupProperties   :=>) groupProperties
        , fmap (NumColumns        :=>) numColumns
        , fmap (StartColumn       :=>) startColumn
        , fmap (RepeatKey         :=>) repeatKey
        , fmap (TextPlacements    :=>) placements
        , GET_BASE_PROPS
        ]
      Debug{..} ->
        [ fmap ((Mode :=>) . Val . Left)  debugMode
        , GET_BASE_PROPS
        ]
      Dot{..} ->
        [ fmap (Fill                 :=>) fill
        , fmap (Opacity              :=>) opacity
        , fmap (Width                :=>) width
        , fmap (Height               :=>) height
        , fmap (CompOp               :=>) compOp
        ]

    stepBase, stepStroke :: Symbolizer -> Property -> Symbolizer
    stepBase !s (SimplifyTolerance :=>v) = s { simplifyTolerance = Just v }
    stepBase !s (Smooth            :=>v) = s { smooth = Just v }
    stepBase !s (Clip              :=>v) = s { clip = Just v }
    stepBase !s (CompOp            :=>v) = s { compOp = Just v }
    stepBase !s (GeometryTransform :=>v) = s { geometryTransform = Just v }
    stepBase !s (SimplifyAlgorithm :=>v) = s { simplifyAlgorithm = Just v }
    stepBase !s _                        = s

    stepStroke !s (StrokeGamma       :=>v) = s { strokeGamma = Just v }
    stepStroke !s (StrokeGammaMethod :=>v) = s { strokeGammaMethod = Just v }
    stepStroke !s (StrokeDasharray   :=>v) = s { strokeDashArray = Just v }
    stepStroke !s (StrokeDashoffset  :=>v) = s { strokeDashOffset = Just v }
    stepStroke !s (StrokeMiterlimit  :=>v) = s { strokeMiterLimit = Just v }
    stepStroke !s (StrokeWidth       :=>v) = s { strokeWidth = Just v }
    stepStroke !s (StrokeOpacity     :=>v) = s { strokeOpacity = Just v }
    stepStroke !s (Stroke            :=>v) = s { stroke = Just v }
    stepStroke !s (StrokeLinejoin    :=>v) = s { strokeLineJoin = Just v }
    stepStroke !s (StrokeLinecap     :=>v) = s { strokeLineCap = Just v }
    stepStroke !s p                        = stepBase s p
