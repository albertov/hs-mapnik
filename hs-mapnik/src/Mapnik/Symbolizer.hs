{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Mapnik.Symbolizer (
  module Mapnik.Symbolizer
, module Mapnik.Symbolizer.Property
, module Mapnik.Symbolizer.TextProperties
, DSum (..)
) where

import Mapnik.Imports
import Mapnik.Common
import Mapnik.Enums
import Mapnik.Symbolizer.TextProperties
import Mapnik.Symbolizer.Property

import Control.Lens
import qualified Data.Dependent.Map as DMap


#define BASE_PROPS \
    simplifyTolerance :: !(PropValue Double) \
  , smooth            :: !(PropValue Double) \
  , clip              :: !(PropValue Bool) \
  , compOp            :: !(PropValue CompositeMode) \
  , geometryTransform :: !(PropValue Transform) \
  , simplifyAlgorithm :: !(PropValue SimplifyAlgorithm)

#define GET_BASE_PROPS \
    SimplifyTolerance :=> simplifyTolerance \
  , Smooth            :=> smooth            \
  , Clip              :=> clip \
  , CompOp            :=> compOp \
  , GeometryTransform :=> geometryTransform \
  , SimplifyAlgorithm :=> simplifyAlgorithm

#define STROKE_PROPS \
    gamma       :: !(PropValue Double) \
  , gammaMethod :: !(PropValue GammaMethod) \
  , dashArray   :: !(PropValue DashArray) \
  , dashOffset  :: !(PropValue Double) \
  , miterLimit  :: !(PropValue Double) \
  , width       :: !(PropValue Double) \
  , opacity     :: !(PropValue Double) \
  , stroke      :: !(PropValue Color) \
  , lineJoin    :: !(PropValue LineJoin) \
  , lineCap     :: !(PropValue LineCap) \
  , BASE_PROPS

#define GET_STROKE_PROPS \
    StrokeGamma       :=> gamma \
  , StrokeGammaMethod :=> gammaMethod \
  , StrokeDasharray   :=> dashArray \
  , StrokeDashoffset  :=> dashOffset \
  , StrokeMiterlimit  :=> miterLimit \
  , StrokeWidth       :=> width \
  , StrokeOpacity     :=> opacity \
  , Stroke            :=> stroke \
  , StrokeLinejoin    :=> lineJoin \
  , StrokeLinecap     :=> lineCap \
  , GET_BASE_PROPS

data Symbolizer
  = Point
    { file            :: !(PropValue FilePath)
    , opacity         :: !(PropValue Double)
    , allowOverlap    :: !(PropValue Bool)
    , ignorePlacement :: !(PropValue Bool)
    , pointPlacement  :: !(PropValue PointPlacement)
    , imageTransform  :: !(PropValue Transform)
    , BASE_PROPS
    }
  | Line
    { offset         :: !(PropValue Double)
    , lineRasterizer :: !(PropValue LineRasterizer)
    , STROKE_PROPS
    }
  | LinePattern
    { file             :: !(PropValue FilePath)
    , opacity          :: !(PropValue Double)
    , offset           :: !(PropValue Double)
    , imageTransform   :: !(PropValue Transform)
    , BASE_PROPS
    }
  | Polygon
    { fill         :: !(PropValue Color)
    , fillOpacity  :: !(PropValue Double)
    , gamma        :: !(PropValue Double)
    , gammaMethod  :: !(PropValue GammaMethod)
    , BASE_PROPS
    }
  | PolygonPattern
    { file            :: !(PropValue FilePath)
    , opacity         :: !(PropValue Double)
    , gamma           :: !(PropValue Double)
    , gammaMethod     :: !(PropValue GammaMethod)
    , imageTransform  :: !(PropValue Transform)
    , alignment       :: !(PropValue PatternAlignment)
    , BASE_PROPS
    }
  -- FIXME: Redesign PropValue since RasterSymbolizer props does not admit expressions
  | Raster
    { rasterMode    :: !(PropValue RasterMode)
    , scaling       :: !(PropValue Scaling)
    , opacity       :: !(PropValue Double)
    , filterFactor  :: !(PropValue Double)
    , meshSize      :: !(PropValue Int)
    , preMultiplied :: !(PropValue Bool)
    , colorizer     :: !(PropValue Colorizer)
    , BASE_PROPS
    }
  | Shield
    { placements      :: !(PropValue TextPlacements)
    , imageTransform  :: !(PropValue Transform)
    , dx              :: !(PropValue Double)
    , dy              :: !(PropValue Double)
    , opacity         :: !(PropValue Double)
    , unlockImage     :: !(PropValue Bool)
    , file            :: !(PropValue FilePath)
    , haloRasterizer  :: !(PropValue HaloRasterizer)
    , BASE_PROPS
    }
  | Text
    { placements     :: !(PropValue TextPlacements)
    , haloCompOp     :: !(PropValue CompositeMode)
    , haloRasterizer :: !(PropValue HaloRasterizer)
    , haloTransform  :: !(PropValue Transform)
    , BASE_PROPS
    }
  | Building
    { fill        :: !(PropValue Color)
    , fillOpacity :: !(PropValue Double)
    , height      :: !(PropValue Double)
    , BASE_PROPS
    }
  | Markers
    { file            :: !(PropValue FilePath)
    , opacity         :: !(PropValue Double)
    , fill            :: !(PropValue Color)
    , fillOpacity     :: !(PropValue Double)
    , spacing         :: !(PropValue Double)
    , maxError        :: !(PropValue Double)
    , offset          :: !(PropValue Double)
    , width           :: !(PropValue Double)
    , height          :: !(PropValue Double)
    , allowOverlap    :: !(PropValue Bool)
    , avoidEdges      :: !(PropValue Bool)
    , ignorePlacement :: !(PropValue Bool)
    , imageTransform  :: !(PropValue Transform)
    , placement       :: !(PropValue MarkerPlacement)
    , multiPolicy     :: !(PropValue MarkerMultiPolicy)
    , direction       :: !(PropValue Direction)
    , STROKE_PROPS
    }
  | Group
    { groupProperties :: !(PropValue GroupProperties)
    , numColumns      :: !(PropValue Int)
    , startColumn     :: !(PropValue Int)
    , repeatKey       :: !(PropValue Expression)
    , placements      :: !(PropValue TextPlacements)
    , BASE_PROPS
    }
  | Debug
    { debugMode :: !(PropValue DebugMode)
    , BASE_PROPS
    }
  | Dot
    { fill    :: !(PropValue Color)
    , opacity :: !(PropValue Double)
    , width   :: !(PropValue Double)
    , height  :: !(PropValue Double)
    , compOp  :: !(PropValue CompositeMode)
    }
  deriving (Eq, Show, Generic)
deriveMapnikJSON ''Symbolizer

class HasProperties s a | s -> a where
  properties :: Lens' s a

instance HasProperties Symbolizer Properties where
  properties = lens getProps setProps where
    setProps sym@Point{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s File               v = s { file = v }
      step !s Opacity            v = s { opacity = v }
      step !s AllowOverlap       v = s { allowOverlap = v }
      step !s IgnorePlacement    v = s { ignorePlacement = v }
      step !s PointPlacementType v = s { pointPlacement = v }
      step !s ImageTransform     v = s { imageTransform = v }
      step !s k                  v = stepBase s k v

    setProps sym@Line{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s Opacity v = s { opacity = v }
      step !s Offset  v = s { offset = v }
      step !s k       v = stepStroke s k v

    setProps sym@LinePattern{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s File            v = s { file = v }
      step !s Opacity         v = s { opacity = v }
      step !s Offset          v = s { offset = v }
      step !s ImageTransform  v = s { imageTransform = v }
      step !s k               v = stepBase s k v

    setProps sym@Polygon{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s Fill        v = s { fill = v }
      step !s FillOpacity v = s { fillOpacity = v }
      step !s Gamma       v = s { gamma = v }
      step !s GammaMethod v = s { gammaMethod = v }
      step !s k           v = stepBase s k v

    setProps sym@PolygonPattern{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s File           v = s { file = v }
      step !s Opacity        v = s { opacity = v }
      step !s Gamma          v = s { gamma = v }
      step !s GammaMethod    v = s { gammaMethod = v }
      step !s ImageTransform v = s { imageTransform = v }
      step !s Alignment      v = s { alignment = v }
      step !s k              v = stepBase s k v

    setProps sym@Raster{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s Mode          (PropValue (Right v)) = s { rasterMode = PropValue v }
      step !s Scaling       v                     = s { scaling = v }
      step !s Opacity       v                     = s { opacity = v }
      step !s FilterFactor  v                     = s { filterFactor = v }
      step !s MeshSize      v                     = s { meshSize = v }
      step !s Premultiplied v                     = s { preMultiplied = v }
      step !s Colorizer     v                     = s { colorizer = v }
      step !s k             v                     = stepBase s k v

    setProps sym@Shield{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s TextPlacements v = s { placements = v }
      step !s ImageTransform v = s { imageTransform = v }
      step !s ShieldDx       v = s { dx = v }
      step !s ShieldDy       v = s { dy = v }
      step !s Opacity        v = s { opacity = v }
      step !s UnlockImage    v = s { unlockImage = v }
      step !s File           v = s { file = v }
      step !s HaloRasterizer v = s { haloRasterizer = v }
      step !s k              v = stepBase s k v

    setProps sym@Text{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s TextPlacements v = s { placements = v }
      step !s HaloCompOp     v = s { haloCompOp = v }
      step !s HaloRasterizer v = s { haloRasterizer = v }
      step !s HaloTransform  v = s { haloTransform = v }
      step !s k              v = stepBase s k v

    setProps sym@Building{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s Fill        v = s { fill = v }
      step !s FillOpacity v = s { fillOpacity = v }
      step !s Height      v = s { height = v }
      step !s k           v = stepBase s k v

    setProps sym@Markers{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s File                  v = s { file = v }
      step !s Opacity               v = s { opacity = v }
      step !s Fill                  v = s { fill = v }
      step !s FillOpacity           v = s { fillOpacity = v }
      step !s Spacing               v = s { spacing = v }
      step !s MaxError              v = s { maxError = v }
      step !s Offset                v = s { offset = v }
      step !s Width                 v = s { width = v }
      step !s Height                v = s { height = v }
      step !s AllowOverlap          v = s { allowOverlap = v }
      step !s AvoidEdges            v = s { avoidEdges = v }
      step !s IgnorePlacement       v = s { ignorePlacement = v }
      step !s ImageTransform        v = s { imageTransform = v }
      step !s MarkersPlacementType  v = s { placement = v }
      step !s MarkersMultipolicy    v = s { multiPolicy = v }
      step !s Direction             v = s { direction = v }
      step !s k                     v = stepStroke s k v

    setProps sym@Group{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s GroupProperties       v = s { groupProperties = v }
      step !s NumColumns            v = s { numColumns = v }
      step !s StartColumn           v = s { startColumn = v }
      step !s RepeatKey             v = s { repeatKey = v }
      step !s TextPlacements        v = s { placements = v }
      step !s k                     v = stepBase s k v

    setProps sym@Debug{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s Mode (PropValue (Left v)) = s { debugMode = PropValue v }
      step !s k                       v = stepBase s k v

    setProps sym@Dot{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> PropValue v -> Symbolizer
      step !s Fill    v = s { fill = v }
      step !s Opacity v = s { opacity = v }
      step !s Width   v = s { width = v }
      step !s Height  v = s { height = v }
      step !s CompOp  v = s { compOp = v }
      step !s _       _ = s

    getProps sym = DMap.fromList . filter (not . isDefault) $ case sym of
      Point{..} ->
        [ File               :=> file
        , Opacity            :=> opacity
        , AllowOverlap       :=> allowOverlap
        , IgnorePlacement    :=> ignorePlacement
        , PointPlacementType :=> pointPlacement
        , ImageTransform     :=> imageTransform
        , GET_BASE_PROPS
        ]
      Line{..} ->
        [ Offset             :=> offset
        , LineRasterizer     :=> lineRasterizer
        , GET_STROKE_PROPS
        ]
      LinePattern{..} ->
        [ File           :=> file
        , Opacity        :=> opacity
        , Offset         :=> offset
        , ImageTransform :=> imageTransform
        , GET_BASE_PROPS
        ]

    isDefault :: Property -> Bool
    isDefault (_ :=> PropDefault) = True
    isDefault _                   = False

    stepBase, stepStroke :: Symbolizer -> Key v -> PropValue v -> Symbolizer
    stepBase !s SimplifyTolerance v = s { simplifyTolerance = v }
    stepBase !s Smooth            v = s { smooth = v }
    stepBase !s Clip              v = s { clip = v }
    stepBase !s CompOp            v = s { compOp = v }
    stepBase !s GeometryTransform v = s { geometryTransform = v }
    stepBase !s SimplifyAlgorithm v = s { simplifyAlgorithm = v }
    stepBase !s _                 _ = s

    stepStroke !s StrokeGamma       v = s { gamma = v }
    stepStroke !s StrokeGammaMethod v = s { gammaMethod = v }
    stepStroke !s StrokeDasharray   v = s { dashArray = v }
    stepStroke !s StrokeDashoffset  v = s { dashOffset = v }
    stepStroke !s StrokeMiterlimit  v = s { miterLimit = v }
    stepStroke !s StrokeWidth       v = s { width = v }
    stepStroke !s StrokeOpacity     v = s { opacity = v }
    stepStroke !s Stroke            v = s { stroke = v }
    stepStroke !s StrokeLinejoin    v = s { lineJoin = v }
    stepStroke !s StrokeLinecap     v = s { lineCap = v }
    stepStroke !s k                 v = stepBase s k v


toProperties :: Symbolizer -> [DSum Key PropValue]
toProperties = view (properties . to DMap.toList)
