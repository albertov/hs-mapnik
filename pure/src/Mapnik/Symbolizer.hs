{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Mapnik.Symbolizer (
  module Mapnik.Symbolizer
, def
) where

import Mapnik.Imports
import Mapnik.Common
import Mapnik.Enums

import Data.Text (Text)
import Data.Default (Default(def))


data Prop a = Exp Expression
            | Val a
  deriving (Eq, Show, Functor, Generic)
instance FromJSON a => FromJSON (Prop a) where
  parseJSON = genericParseJSON mapnikOptions
instance ToJSON a => ToJSON (Prop a) where
  toJSON = genericToJSON mapnikOptions
  toEncoding = genericToEncoding mapnikOptions

type PropValue a = Maybe (Prop a)

type FaceName = Text

data Stop = Stop
  { value :: !Double
  , color :: !Color
  , mode  :: !(Maybe ColorizerMode)
  , label :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

data Colorizer = Colorizer
  { mode   :: !(Maybe ColorizerMode)
  , color  :: !(Maybe Color)
  , stops  :: ![Stop]
  } deriving (Eq, Show, Generic)

data GroupLayout
  = SimpleRowLayout { itemMargin    :: !(Maybe Double) }
  | PairLayout      { itemMargin    :: !(Maybe Double)
                    , maxDifference :: !(Maybe Double) }
  deriving (Eq, Show, Generic)

instance Default GroupLayout where def = SimpleRowLayout def

data GroupRule = GroupRule
  { symbolizers :: ![Symbolizer]
  , filter      :: !(Maybe Expression)
  , repeatKey   :: !(Maybe Expression)
  } deriving (Eq, Show, Generic, Default)

data GroupSymProperties = GroupSymProperties
  { layout :: !GroupLayout
  , rules  :: ![GroupRule]
  }
  deriving (Eq, Show, Generic)

data FontSet = FontSet
  deriving (Eq, Show, Generic)


data TextProperties = TextProperties
  { labelPlacement          :: !(PropValue LabelPlacement)
  , labelSpacing            :: !(PropValue Double)
  , labelPositionTolerance  :: !(PropValue Double)
  , avoidEdges              :: !(PropValue Bool)
  , margin                  :: !(PropValue Double)
  , repeatDistance          :: !(PropValue Double)
  , minimumDistance         :: !(PropValue Double) -- Deprecated
  , minimumPadding          :: !(PropValue Double)
  , minimumPathLength       :: !(PropValue Double)
  , maxCharAngleDelta       :: !(PropValue Double)
  , allowOverlap            :: !(PropValue Bool)
  , largestBoxOnly          :: !(PropValue Bool)
  , upright                 :: !(PropValue Upright)
  } deriving (Eq, Show, Generic, Default)

data TextFormatProperties = TextFormatProperties
  { faceName         :: !(Maybe FaceName)
  , fontSet          :: !(Maybe FontSet)
  , textSize         :: !(PropValue Double)
  , characterSpacing :: !(PropValue Double)
  , lineSpacing      :: !(PropValue Double)
  , textOpacity      :: !(PropValue Double)
  , haloOpacity      :: !(PropValue Double)
  , textTransform    :: !(PropValue TextTransform)
  , fill             :: !(PropValue Color)
  , haloFill         :: !(PropValue Color)
  , haloRadius       :: !(PropValue Double)
  , ffSettings       :: !(PropValue FontFeatureSettings)
  } deriving (Eq, Show, Generic, Default)

data TextLayoutProperties = TextLayoutProperties
  { dx                  :: !(PropValue Double)
  , dy                  :: !(PropValue Double)
  , orientation         :: !(PropValue Double)
  , textRatio           :: !(PropValue Double)
  , wrapWidth           :: !(PropValue Double)
  , wrapChar            :: !(PropValue Char)
  , wrapBefore          :: !(PropValue Bool)
  , repeatWrapChar      :: !(PropValue Bool)
  , rotateDisplacement  :: !(PropValue Double)
  , horizontalAlignment :: !(PropValue HorizontalAlignment)
  , justifyAlignment    :: !(PropValue JustifyAlignment)
  , verticalAlignment   :: !(PropValue VerticalAlignment)
  , direction           :: !(Maybe PlacementDirection)
  } deriving (Eq, Show, Generic, Default)

data Format
  = FormatExp    !Expression
  | FormatList   ![Format]
  | Format
    { faceName         :: !(Maybe FaceName)
    , fontSet          :: !(Maybe FontSet)
    , textSize         :: !(PropValue Double)
    , characterSpacing :: !(PropValue Double)
    , lineSpacing      :: !(PropValue Double)
    , wrapBefore       :: !(PropValue Bool)
    , repeatWrapChar   :: !(PropValue Bool)
    , textTransform    :: !(PropValue TextTransform)
    , fill             :: !(PropValue Color)
    , haloFill         :: !(PropValue Color)
    , haloRadius       :: !(PropValue Double)
    , ffSettings       :: !(PropValue FontFeatureSettings)
    , next             :: !Format
    }
  | FormatLayout
    { dx                  :: !(PropValue Double)
    , dy                  :: !(PropValue Double)
    , orientation         :: !(PropValue Double)
    , textRatio           :: !(PropValue Double)
    , wrapWidth           :: !(PropValue Double)
    , wrapChar            :: !(PropValue Char)
    , wrapBefore          :: !(PropValue Bool)
    , repeatWrapChar      :: !(PropValue Bool)
    , rotateDisplacement  :: !(PropValue Double)
    , horizontalAlignment :: !(PropValue HorizontalAlignment)
    , justifyAlignment    :: !(PropValue JustifyAlignment)
    , verticalAlignment   :: !(PropValue VerticalAlignment)
    , next                :: !Format
    }
  | NullFormat
  deriving (Eq, Show, Generic)

instance Default Format where def = NullFormat

data TextSymProperties = TextSymProperties
  { properties       :: !TextProperties
  , layoutProperties :: !TextLayoutProperties
  , formatProperties :: !TextFormatProperties
  , format           :: !Format
  } deriving (Eq, Show, Generic, Default)


newtype TextPlacements = Dummy TextSymProperties
  deriving (Generic)
  deriving newtype (Eq, Show, Default)

#define BASE_PROPS \
    simplifyTolerance :: !(PropValue Double) \
  , smooth            :: !(PropValue Double) \
  , clip              :: !(PropValue Bool) \
  , compOp            :: !(PropValue CompositeMode) \
  , geometryTransform :: !(PropValue Transform) \
  , simplifyAlgorithm :: !(PropValue SimplifyAlgorithm)


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
  = PointSymbolizer
    { file            :: !(PropValue FilePath)
    , opacity         :: !(PropValue Double)
    , allowOverlap    :: !(PropValue Bool)
    , ignorePlacement :: !(PropValue Bool)
    , pointPlacement  :: !(PropValue PointPlacement)
    , imageTransform  :: !(PropValue Transform)
    , BASE_PROPS
    }
  | LineSymbolizer
    { offset         :: !(PropValue Double)
    , lineRasterizer :: !(PropValue LineRasterizer)
    , STROKE_PROPS
    }
  | LinePatternSymbolizer
    { file             :: !(PropValue FilePath)
    , opacity          :: !(PropValue Double)
    , offset           :: !(PropValue Double)
    , imageTransform   :: !(PropValue Transform)
    , BASE_PROPS
    }
  | PolygonSymbolizer
    { fill         :: !(PropValue Color)
    , fillOpacity  :: !(PropValue Double)
    , gamma        :: !(PropValue Double)
    , gammaMethod  :: !(PropValue GammaMethod)
    , BASE_PROPS
    }
  | PolygonPatternSymbolizer
    { file            :: !(PropValue FilePath)
    , opacity         :: !(PropValue Double)
    , gamma           :: !(PropValue Double)
    , gammaMethod     :: !(PropValue GammaMethod)
    , imageTransform  :: !(PropValue Transform)
    , alignment       :: !(PropValue PatternAlignment)
    , BASE_PROPS
    }
  | RasterSymbolizer
    { scaling       :: !(Maybe ScalingMethod)
    , rasterOpacity :: !(Maybe Double)
    , filterFactor  :: !(Maybe Double)
    , meshSize      :: !(Maybe Int)
    , preMultiplied :: !(Maybe Bool)
    , colorizer     :: !(Maybe Colorizer)
    , BASE_PROPS
    }
  | ShieldSymbolizer
    { placements      :: !(Maybe TextPlacements)
    , imageTransform  :: !(PropValue Transform)
    , dx              :: !(PropValue Double)
    , dy              :: !(PropValue Double)
    , opacity         :: !(PropValue Double)
    , unlockImage     :: !(PropValue Bool)
    , file            :: !(PropValue FilePath)
    , haloRasterizer  :: !(PropValue HaloRasterizer)
    , BASE_PROPS
    }
  | TextSymbolizer
    { placements     :: !(Maybe TextPlacements)
    , haloCompOp     :: !(PropValue CompositeMode)
    , haloRasterizer :: !(PropValue HaloRasterizer)
    , haloTransform  :: !(PropValue Transform)
    , BASE_PROPS
    }
  | BuildingSymbolizer
    { fill        :: !(PropValue Color)
    , fillOpacity :: !(PropValue Double)
    , height      :: !(PropValue Double)
    , BASE_PROPS
    }
  | MarkersSymbolizer
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
  | GroupSymbolizer
    { groupProperties :: !(PropValue GroupSymProperties)
    , numColumns      :: !(PropValue Int)
    , startColumn     :: !(PropValue Int)
    , repeatKey       :: !(PropValue Expression)
    , placements      :: !(Maybe TextPlacements)
    , BASE_PROPS
    }
  | DebugSymbolizer
    { mode            :: !(Maybe DebugMode)
    , BASE_PROPS
    }
  | DotSymbolizer
    { fill    :: !(PropValue Color)
    , opacity :: !(PropValue Double)
    , width   :: !(PropValue Double)
    , height  :: !(PropValue Double)
    , compOp  :: !(PropValue CompositeMode)
    }
  deriving (Eq, Show, Generic)


pointSym, lineSym, linePatternSym, polygonSym, polygonPatternSym, rasterSym :: Symbolizer
shieldSym, textSym, buildingSym, markersSym, groupSym, debugSym, dotSym :: Symbolizer
pointSym = PointSymbolizer
  { file            = Nothing
  , opacity         = Nothing
  , allowOverlap    = Nothing
  , ignorePlacement = Nothing
  , pointPlacement  = Nothing
  , imageTransform  = Nothing
  , BASE_PROPS_DEFS
  }
lineSym = LineSymbolizer
  { offset         = Nothing
  , lineRasterizer = Nothing
  , STROKE_PROPS_DEFS
  }
linePatternSym = LinePatternSymbolizer
  { file             = Nothing
  , opacity          = Nothing
  , offset           = Nothing
  , imageTransform   = Nothing
  , BASE_PROPS_DEFS
  }
polygonSym = PolygonSymbolizer
  { fill         = Nothing
  , fillOpacity  = Nothing
  , gamma        = Nothing
  , gammaMethod  = Nothing
  , BASE_PROPS_DEFS
  }
polygonPatternSym = PolygonPatternSymbolizer
  { file            = Nothing
  , opacity         = Nothing
  , gamma           = Nothing
  , gammaMethod     = Nothing
  , imageTransform  = Nothing
  , alignment       = Nothing
  , BASE_PROPS_DEFS
  }
rasterSym = RasterSymbolizer
  { scaling       = Nothing
  , rasterOpacity = Nothing
  , filterFactor  = Nothing
  , meshSize      = Nothing
  , preMultiplied = Nothing
  , colorizer     = Nothing
  , BASE_PROPS_DEFS
  }
shieldSym = ShieldSymbolizer
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
textSym = TextSymbolizer
  { placements     = Nothing
  , haloCompOp     = Nothing
  , haloRasterizer = Nothing
  , haloTransform  = Nothing
  , BASE_PROPS_DEFS
  }
buildingSym = BuildingSymbolizer
  { fill        = Nothing
  , fillOpacity = Nothing
  , height      = Nothing
  , BASE_PROPS_DEFS
  }
markersSym = MarkersSymbolizer
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
groupSym = GroupSymbolizer
  { groupProperties = Nothing
  , numColumns      = Nothing
  , startColumn     = Nothing
  , repeatKey       = Nothing
  , placements      = Nothing
  , BASE_PROPS_DEFS
  }
debugSym = DebugSymbolizer
  { mode = Nothing
  , BASE_PROPS_DEFS
  }
dotSym = DotSymbolizer
  { fill    = Nothing
  , opacity = Nothing
  , width   = Nothing
  , height  = Nothing
  , compOp  = Nothing
  }

deriveMapnikJSON ''Symbolizer
deriveMapnikJSON ''TextPlacements
deriveMapnikJSON ''TextSymProperties
deriveMapnikJSON ''GroupSymProperties
deriveMapnikJSON ''Format
deriveMapnikJSON ''TextLayoutProperties
deriveMapnikJSON ''TextFormatProperties
deriveMapnikJSON ''TextProperties
deriveMapnikJSON ''FontSet
deriveMapnikJSON ''Stop
deriveMapnikJSON ''Colorizer
deriveMapnikJSON ''GroupLayout
deriveMapnikJSON ''GroupRule
