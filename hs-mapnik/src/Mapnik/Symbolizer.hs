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
  | Raster
    { rasterMode    :: !(Maybe RasterMode)
    , scaling       :: !(Maybe Scaling)
    , rasterOpacity :: !(Maybe Double)
    , filterFactor  :: !(Maybe Double)
    , meshSize      :: !(Maybe Int)
    , preMultiplied :: !(Maybe Bool)
    , colorizer     :: !(Maybe Colorizer)
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
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s File               v = s { file = Just v }
      step !s Opacity            v = s { opacity = Just v }
      step !s AllowOverlap       v = s { allowOverlap = Just v }
      step !s IgnorePlacement    v = s { ignorePlacement = Just v }
      step !s PointPlacementType v = s { pointPlacement = Just v }
      step !s ImageTransform     v = s { imageTransform = Just v }
      step !s k                  v = stepBase s k v

    setProps sym@Line{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s Opacity v = s { opacity = Just v }
      step !s Offset  v = s { offset = Just v }
      step !s k       v = stepStroke s k v

    setProps sym@LinePattern{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s File            v = s { file = Just v }
      step !s Opacity         v = s { opacity = Just v }
      step !s Offset          v = s { offset = Just v }
      step !s ImageTransform  v = s { imageTransform = Just v }
      step !s k               v = stepBase s k v

    setProps sym@Polygon{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s Fill        v = s { fill = Just v }
      step !s FillOpacity v = s { fillOpacity = Just v }
      step !s Gamma       v = s { gamma = Just v }
      step !s GammaMethod v = s { gammaMethod = Just v }
      step !s k           v = stepBase s k v

    setProps sym@PolygonPattern{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s File           v = s { file = Just v }
      step !s Opacity        v = s { opacity = Just v }
      step !s Gamma          v = s { gamma = Just v }
      step !s GammaMethod    v = s { gammaMethod = Just v }
      step !s ImageTransform v = s { imageTransform = Just v }
      step !s Alignment      v = s { alignment = Just v }
      step !s k              v = stepBase s k v

    setProps sym@Raster{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s Mode          (Val (Right v)) = s { rasterMode = Just v }
      step !s Scaling       (Val v) = s { scaling = Just v }
      step !s Opacity       (Val v) = s { rasterOpacity = Just v }
      step !s FilterFactor  (Val v) = s { filterFactor = Just v }
      step !s MeshSize      (Val v) = s { meshSize = Just v }
      step !s Premultiplied (Val v) = s { preMultiplied = Just v }
      step !s Colorizer     (Val v) = s { colorizer = Just v }
      step !s k                   v = stepBase s k v

    setProps sym@Shield{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s TextPlacements v = s { placements = Just v }
      step !s ImageTransform v = s { imageTransform = Just v }
      step !s ShieldDx       v = s { dx = Just v }
      step !s ShieldDy       v = s { dy = Just v }
      step !s Opacity        v = s { opacity = Just v }
      step !s UnlockImage    v = s { unlockImage = Just v }
      step !s File           v = s { file = Just v }
      step !s HaloRasterizer v = s { haloRasterizer = Just v }
      step !s k              v = stepBase s k v

    setProps sym@Text{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s TextPlacements v = s { placements = Just v }
      step !s HaloCompOp     v = s { haloCompOp = Just v }
      step !s HaloRasterizer v = s { haloRasterizer = Just v }
      step !s HaloTransform  v = s { haloTransform = Just v }
      step !s k              v = stepBase s k v

    setProps sym@Building{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s Fill        v = s { fill = Just v }
      step !s FillOpacity v = s { fillOpacity = Just v }
      step !s Height      v = s { height = Just v }
      step !s k           v = stepBase s k v

    setProps sym@Markers{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s File                  v = s { file = Just v }
      step !s Opacity               v = s { opacity = Just v }
      step !s Fill                  v = s { fill = Just v }
      step !s FillOpacity           v = s { fillOpacity = Just v }
      step !s Spacing               v = s { spacing = Just v }
      step !s MaxError              v = s { maxError = Just v }
      step !s Offset                v = s { offset = Just v }
      step !s Width                 v = s { width = Just v }
      step !s Height                v = s { height = Just v }
      step !s AllowOverlap          v = s { allowOverlap = Just v }
      step !s AvoidEdges            v = s { avoidEdges = Just v }
      step !s IgnorePlacement       v = s { ignorePlacement = Just v }
      step !s ImageTransform        v = s { imageTransform = Just v }
      step !s MarkersPlacementType  v = s { placement = Just v }
      step !s MarkersMultipolicy    v = s { multiPolicy = Just v }
      step !s Direction             v = s { direction = Just v }
      step !s k                     v = stepStroke s k v

    setProps sym@Group{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s GroupProperties       v = s { groupProperties = Just v }
      step !s NumColumns            v = s { numColumns = Just v }
      step !s StartColumn           v = s { startColumn = Just v }
      step !s RepeatKey             v = s { repeatKey = Just v }
      step !s TextPlacements        v = s { placements = Just v }
      step !s k                     v = stepBase s k v

    setProps sym@Debug{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s Mode (Val (Left v)) = s { debugMode = PropValue v }
      step !s k                   v = stepBase s k v

    setProps sym@Dot{} = DMap.foldlWithKey step sym  where
      step :: Symbolizer -> Key v -> Prop v -> Symbolizer
      step !s Fill    v = s { fill = Just v }
      step !s Opacity v = s { opacity = Just v }
      step !s Width   v = s { width = Just v }
      step !s Height  v = s { height = Just v }
      step !s CompOp  v = s { compOp = Just v }
      step !s _       _ = s

    getProps = undefined
    {-
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
        -}

    stepBase, stepStroke :: Symbolizer -> Key v -> Prop v -> Symbolizer
    stepBase !s SimplifyTolerance v = s { simplifyTolerance = Just v }
    stepBase !s Smooth            v = s { smooth = Just v }
    stepBase !s Clip              v = s { clip = Just v }
    stepBase !s CompOp            v = s { compOp = Just v }
    stepBase !s GeometryTransform v = s { geometryTransform = Just v }
    stepBase !s SimplifyAlgorithm v = s { simplifyAlgorithm = Just v }
    stepBase !s _                 _ = s

    stepStroke !s StrokeGamma       v = s { gamma = Just v }
    stepStroke !s StrokeGammaMethod v = s { gammaMethod = Just v }
    stepStroke !s StrokeDasharray   v = s { dashArray = Just v }
    stepStroke !s StrokeDashoffset  v = s { dashOffset = Just v }
    stepStroke !s StrokeMiterlimit  v = s { miterLimit = Just v }
    stepStroke !s StrokeWidth       v = s { width = Just v }
    stepStroke !s StrokeOpacity     v = s { opacity = Just v }
    stepStroke !s Stroke            v = s { stroke = Just v }
    stepStroke !s StrokeLinejoin    v = s { lineJoin = Just v }
    stepStroke !s StrokeLinecap     v = s { lineCap = Just v }
    stepStroke !s k                 v = stepBase s k v


toProperties :: Symbolizer -> [DSum Key Prop]
toProperties = view (properties . to DMap.toList)
