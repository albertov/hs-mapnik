{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
