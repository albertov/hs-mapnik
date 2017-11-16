{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Mapnik.Symbolizer (
  module Mapnik.Symbolizer
, module Mapnik.Symbolizer.Property
) where

import Mapnik.Imports
import Mapnik.Common
import Mapnik.Enums
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
    { groupProperties :: !(PropValue GroupProperties)
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
deriveMapnikJSON ''Symbolizer


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
