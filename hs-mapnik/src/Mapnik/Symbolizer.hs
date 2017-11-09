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
) where

import Mapnik.Imports
import Mapnik.Common
import Mapnik.Enums
import Mapnik.Symbolizer.TextProperties
import Mapnik.Symbolizer.Property

import Control.Lens
import Data.Maybe (catMaybes)
import Data.List (foldl')


#define BASE_PROPS \
    simplifyTolerance :: !(PropValue Double) \
  , smooth            :: !(PropValue Double) \
  , clip              :: !(PropValue Bool) \
  , compOp            :: !(PropValue CompositeMode) \
  , geometryTransform :: !(PropValue Transform) \
  , simplifyAlgorithm :: !(PropValue SimplifyAlgorithm)

#define GET_BASE_PROPS \
    fmap (SimplifyTolerance :=>) simplifyTolerance \
  , fmap (Smooth            :=>) smooth            \
  , fmap (Clip              :=>) clip \
  , fmap (CompOp            :=>) compOp \
  , fmap (GeometryTransform :=>) geometryTransform \
  , fmap (SimplifyAlgorithm :=>) simplifyAlgorithm

#define STROKE_PROPS \
    strokeGamma       :: !(PropValue Double) \
  , strokeGammaMethod :: !(PropValue GammaMethod) \
  , strokeDashArray   :: !(PropValue DashArray) \
  , strokeDashOffset  :: !(PropValue Double) \
  , strokeMiterLimit  :: !(PropValue Double) \
  , strokeWidth       :: !(PropValue Double) \
  , strokeOpacity     :: !(PropValue Double) \
  , stroke            :: !(PropValue Color) \
  , strokeLineJoin    :: !(PropValue LineJoin) \
  , strokeLineCap     :: !(PropValue LineCap) \
  , BASE_PROPS

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

#define BASE_PROPS_DEFS \
    simplifyTolerance = Nothing \
  , smooth            = Nothing \
  , clip              = Nothing \
  , compOp            = Nothing \
  , geometryTransform = Nothing \
  , simplifyAlgorithm = Nothing

#define STROKE_PROPS_DEFS \
    strokeGamma       = Nothing \
  , strokeGammaMethod = Nothing \
  , strokeDashArray   = Nothing \
  , strokeDashOffset  = Nothing \
  , strokeMiterLimit  = Nothing \
  , strokeWidth       = Nothing \
  , strokeOpacity     = Nothing \
  , stroke            = Nothing \
  , strokeLineJoin    = Nothing \
  , strokeLineCap     = Nothing \
  , BASE_PROPS_DEFS

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
    { debugMode :: !(Maybe DebugMode)
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

-- We don't use TH From/ToJSONderivation here because it
-- doesn't respect omitNothingFields=True
instance ToJSON Symbolizer where
  toJSON = genericToJSON mapnikOptions
  toEncoding = genericToEncoding mapnikOptions
instance FromJSON Symbolizer where
  parseJSON = genericParseJSON mapnikOptions

point, line, linePattern, polygon, polygonPattern, raster :: Symbolizer
shield, text, building, markers, group, debug, dot :: Symbolizer
point = Point
  { file            = Nothing
  , opacity         = Nothing
  , allowOverlap    = Nothing
  , ignorePlacement = Nothing
  , pointPlacement  = Nothing
  , imageTransform  = Nothing
  , BASE_PROPS_DEFS
  }
line = Line
  { offset         = Nothing
  , lineRasterizer = Nothing
  , STROKE_PROPS_DEFS
  }
linePattern = LinePattern
  { file             = Nothing
  , opacity          = Nothing
  , offset           = Nothing
  , imageTransform   = Nothing
  , BASE_PROPS_DEFS
  }
polygon = Polygon
  { fill         = Nothing
  , fillOpacity  = Nothing
  , gamma        = Nothing
  , gammaMethod  = Nothing
  , BASE_PROPS_DEFS
  }
polygonPattern = PolygonPattern
  { file            = Nothing
  , opacity         = Nothing
  , gamma           = Nothing
  , gammaMethod     = Nothing
  , imageTransform  = Nothing
  , alignment       = Nothing
  , BASE_PROPS_DEFS
  }
raster = Raster
  { rasterMode    = Nothing
  , scaling       = Nothing
  , rasterOpacity = Nothing
  , filterFactor  = Nothing
  , meshSize      = Nothing
  , preMultiplied = Nothing
  , colorizer     = Nothing
  , BASE_PROPS_DEFS
  }
shield = Shield
  { placements      = Nothing
  , imageTransform  = Nothing
  , dx              = Nothing
  , dy              = Nothing
  , opacity         = Nothing
  , unlockImage     = Nothing
  , file            = Nothing
  , haloRasterizer  = Nothing
  , BASE_PROPS_DEFS
  }
text = Text
  { placements     = Nothing
  , haloCompOp     = Nothing
  , haloRasterizer = Nothing
  , haloTransform  = Nothing
  , BASE_PROPS_DEFS
  }
building = Building
  { fill        = Nothing
  , fillOpacity = Nothing
  , height      = Nothing
  , BASE_PROPS_DEFS
  }
markers = Markers
  { file            = Nothing
  , opacity         = Nothing
  , fill            = Nothing
  , fillOpacity     = Nothing
  , spacing         = Nothing
  , maxError        = Nothing
  , offset          = Nothing
  , width           = Nothing
  , height          = Nothing
  , allowOverlap    = Nothing
  , avoidEdges      = Nothing
  , ignorePlacement = Nothing
  , imageTransform  = Nothing
  , placement       = Nothing
  , multiPolicy     = Nothing
  , direction       = Nothing
  , STROKE_PROPS_DEFS
  }
group = Group
  { groupProperties = Nothing
  , numColumns      = Nothing
  , startColumn     = Nothing
  , repeatKey       = Nothing
  , placements      = Nothing
  , BASE_PROPS_DEFS
  }
debug = Debug
  { debugMode = Nothing
  , BASE_PROPS_DEFS
  }
dot = Dot
  { fill    = Nothing
  , opacity = Nothing
  , width   = Nothing
  , height  = Nothing
  , compOp  = Nothing
  }

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


toProperties :: Symbolizer -> Properties
toProperties = view properties
