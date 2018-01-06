{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
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
module Mapnik.Symbolizer where

import Mapnik.Imports
import Mapnik.Common
import Mapnik.Color
import Mapnik.Enums

import Control.Applicative ((<|>), optional)
import Control.Monad (unless, void, when)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Default (Default(def))

import GHC.Exts (IsList(..))

-- | A 'Prop' are symbolizer properties which can be either a fixed-value 'Val' or
-- a feature-dependent 'Expression' 'Exp'
data Prop a = Exp Expression
            | Val a
  deriving (Eq, Show, Functor, Generic)
instance FromJSON a => FromJSON (Prop a) where
  parseJSON = genericParseJSON mapnikOptions
instance ToJSON a => ToJSON (Prop a) where
  toJSON = genericToJSON mapnikOptions
  toEncoding = genericToEncoding mapnikOptions

-- | Nullable properties. When 'Nothing', mapnik will use a default value
type PropValue a = Maybe (Prop a)

data Stop = Stop
  { value :: !Float
  , color :: !Color
  , mode  :: !(Maybe ColorizerMode)
  , label :: !(Maybe Text)
  } deriving (Eq, Show, Generic)

stop :: Float -> Color -> Stop
stop v c = Stop v c Nothing Nothing

-- | See <https://github.com/mapnik/mapnik/wiki/RasterColorizer>
data Colorizer = Colorizer
  { mode   :: !(Maybe ColorizerMode)
  , color  :: !(Maybe Color)
  , epsilon :: !(Maybe Float)
  , stops  :: ![Stop]
  } deriving (Eq, Show, Generic, Default)

instance IsList Colorizer where
  type Item Colorizer = Stop
  fromList = Colorizer Nothing Nothing Nothing
  toList   = stops

-- | See <https://github.com/mapnik/mapnik/wiki/GroupSymbolizer>
data GroupLayout
  = SimpleRowLayout { itemMargin    :: !(Maybe Double) }
  | PairLayout      { itemMargin    :: !(Maybe Double)
                    , maxDifference :: !(Maybe Double) }
  deriving (Eq, Show, Generic)

rowLayout :: GroupLayout
rowLayout = SimpleRowLayout Nothing

pairLayout :: GroupLayout
pairLayout = PairLayout Nothing Nothing

instance Default GroupLayout where def = rowLayout

-- | See <https://github.com/mapnik/mapnik/wiki/GroupSymbolizer>
data GroupRule = GroupRule
  { symbolizers :: ![Symbolizer]
  , filter      :: !(Maybe Expression)
  , repeatKey   :: !(Maybe Expression)
  } deriving (Eq, Show, Generic, Default)

-- | See <https://github.com/mapnik/mapnik/wiki/GroupSymbolizer>
data GroupSymProperties = GroupSymProperties
  { layout :: !GroupLayout
  , rules  :: ![GroupRule]
  } deriving (Eq, Show, Generic, Default)


data Font
  = FontSetName !FontSetName
  | FaceName    !FaceName
  deriving (Eq, Show, Generic)

-- | See <https://github.com/mapnik/mapnik/wiki/TextSymbolizer>
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

-- | <See https://github.com/mapnik/mapnik/wiki/TextSymbolizer#character-formatting-options>
data TextFormatProperties = TextFormatProperties
  { font         :: !(Maybe Font)
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

-- | See https://github.com/mapnik/mapnik/wiki/TextSymbolizer#text-layout-options>
data TextLayoutProperties = TextLayoutProperties
  { dx                  :: !(PropValue Double)
  , dy                  :: !(PropValue Double)
  , orientation         :: !(PropValue Double)
  , textRatio           :: !(PropValue Double)
  , wrapWidth           :: !(PropValue Double)
  , wrapChar            :: !(PropValue Char)
  , wrapBefore          :: !(PropValue Bool)
  , repeatWrapChar      :: !(PropValue Bool)
  , rotateDisplacement  :: !(PropValue Bool)
  , horizontalAlignment :: !(PropValue HorizontalAlignment)
  , justifyAlignment    :: !(PropValue JustifyAlignment)
  , verticalAlignment   :: !(PropValue VerticalAlignment)
  , direction           :: !(Maybe PlacementDirection)
  } deriving (Eq, Show, Generic, Default)

data Format
  = FormatExp    !Expression
  | FormatList   ![Format]
  -- | See <https://github.com/mapnik/mapnik/wiki/TextSymbolizer#formats>
  | Format
    { font             :: !(Maybe Font)
    , textSize         :: !(PropValue Double)
    , opacity          :: !(PropValue Double)
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
  -- | See <https://github.com/mapnik/mapnik/wiki/TextSymbolizer#layouts>
  | FormatLayout
    { dx                  :: !(PropValue Double)
    , dy                  :: !(PropValue Double)
    , orientation         :: !(PropValue Double)
    , textRatio           :: !(PropValue Double)
    , wrapWidth           :: !(PropValue Double)
    , wrapChar            :: !(PropValue Char)
    , wrapBefore          :: !(PropValue Bool)
    , repeatWrapChar      :: !(PropValue Bool)
    , rotateDisplacement  :: !(PropValue Bool)
    , horizontalAlignment :: !(PropValue HorizontalAlignment)
    , justifyAlignment    :: !(PropValue JustifyAlignment)
    , verticalAlignment   :: !(PropValue VerticalAlignment)
    , next                :: !Format
    }
  | NullFormat
  deriving (Eq, Show, Generic)

-- See 'FormatExp'
formatExp :: Expression -> Format
formatExp = FormatExp

-- See 'FormatList'
formatList :: [Format] -> Format
formatList = FormatList

-- See 'Format'
format_ :: Format
format_ = Format
  { font             = Nothing
  , textSize         = Nothing
  , opacity          = Nothing
  , characterSpacing = Nothing
  , lineSpacing      = Nothing
  , wrapBefore       = Nothing
  , repeatWrapChar   = Nothing
  , textTransform    = Nothing
  , fill             = Nothing
  , haloFill         = Nothing
  , haloRadius       = Nothing
  , ffSettings       = Nothing
  , next             = NullFormat
  }

-- See 'FormatLayout'
formatLayout :: Format
formatLayout = FormatLayout
  { dx                  = Nothing
  , dy                  = Nothing
  , orientation         = Nothing
  , textRatio           = Nothing
  , wrapWidth           = Nothing
  , wrapChar            = Nothing
  , wrapBefore          = Nothing
  , repeatWrapChar      = Nothing
  , rotateDisplacement  = Nothing
  , horizontalAlignment = Nothing
  , justifyAlignment    = Nothing
  , verticalAlignment   = Nothing
  , next                = NullFormat
  }


instance IsList Format where
  type Item Format = Format
  fromList = formatList
  toList (FormatList x) = x
  toList _ = []

instance Default Format where def = NullFormat

data TextSymProperties = TextSymProperties
  { properties       :: !TextProperties
  , layoutProperties :: !TextLayoutProperties
  , formatProperties :: !TextFormatProperties
  , format           :: !Format
  } deriving (Eq, Show, Generic, Default)


data SimplePlacementPosition = SimplePlacementPosition
  { textSizes            :: ![Int]
  , directions           :: ![PlacementDirection]
  } deriving (Eq, Show, Generic)

instance Default SimplePlacementPosition where
  def = SimplePlacementPosition [] [PExact]

placementPositionToText :: Prop SimplePlacementPosition -> Text
placementPositionToText (Exp (Expression e)) = e
placementPositionToText (Val (SimplePlacementPosition sizes dirs)) =
  wrap (T.intercalate "," $ map toTextDir dirs ++ map toTextSz sizes)
  where
    -- work around mapnik strainge behaviour
    wrap s
      | length dirs ==1 && null sizes = "'" <> s <> "'"
      | otherwise                     = s
    toTextDir North = "N"
    toTextDir East = "E"
    toTextDir South = "S"
    toTextDir West = "W"
    toTextDir NorthEast = "NE"
    toTextDir SouthEast = "SE"
    toTextDir NorthWest = "NW"
    toTextDir SouthWest = "SW"
    toTextDir PExact = "X"

    toTextSz = T.pack . show

parsePlacementPosition :: Text -> Either String (Prop SimplePlacementPosition)
parsePlacementPosition = parseOnly (
  (   (Val <$> placementParser)
  <|> (Val <$> (char '\'' *> placementParser <* char '\''))
  <|> (Exp . Expression <$> takeText)
  ) <* endOfInput)

placementParser :: Parser SimplePlacementPosition
placementParser = do
  dirs <- fromMaybe [] <$> optional (directionParser `sepBy` char ',')
  sizes <- fromMaybe [] <$> optional (do
    unless (null dirs) (void (char ','))
    decimal `sepBy` char ','
    )
  when (null dirs && null sizes) (fail "no dirs or sizes")
  pure (SimplePlacementPosition sizes dirs)

directionParser :: Parser PlacementDirection
directionParser = choice
  [ "NE" *> pure NorthEast
  , "NW" *> pure NorthWest
  , "SE" *> pure SouthEast
  , "SW" *> pure SouthWest
  , "N"  *> pure North
  , "S"  *> pure South
  , "E"  *> pure East
  , "W"  *> pure West
  , "X"  *> pure PExact
  ]
 
data TextPlacements
  = Simple
    { defaults  :: !TextSymProperties
    , positions :: !(Prop SimplePlacementPosition)
    }
  | List
    { defaults :: !TextSymProperties
    , placements :: ![TextSymProperties]
    }
  | Dummy
    { defaults :: !TextSymProperties
    }
  deriving (Eq, Show, Generic)

simplePlacements, listPlacements, dummyPlacements :: TextPlacements
simplePlacements = Simple def (Val def)
listPlacements = List def def
dummyPlacements = Dummy def

instance Default TextPlacements where def = dummyPlacements


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
    { file            :: !(Maybe PathExpression)
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
    { file             :: !(Maybe PathExpression)
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
    { file            :: !(Maybe PathExpression)
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
    { placements      :: !TextPlacements
    , imageTransform  :: !(PropValue Transform)
    , dx              :: !(PropValue Double)
    , dy              :: !(PropValue Double)
    , opacity         :: !(PropValue Double)
    , unlockImage     :: !(PropValue Bool)
    , file            :: !(Maybe PathExpression)
    , haloRasterizer  :: !(PropValue HaloRasterizer)
    , BASE_PROPS
    }
  | TextSymbolizer
    { placements     :: !TextPlacements
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
    { file            :: !(Maybe PathExpression)
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
    { groupProperties :: !GroupSymProperties
    , numColumns      :: !(PropValue Int)
    , startColumn     :: !(PropValue Int)
    , repeatKey       :: !(PropValue Expression)
    , placements      :: !TextPlacements
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
-- | See <https://github.com/mapnik/mapnik/wiki/PointSymbolizer> and the
-- 'PointSymbolizer' constructor fields for the available properties and their
-- meaning.
pointSym = PointSymbolizer
  { file            = Nothing
  , opacity         = Nothing
  , allowOverlap    = Nothing
  , ignorePlacement = Nothing
  , pointPlacement  = Nothing
  , imageTransform  = Nothing
  , BASE_PROPS_DEFS
  }
-- | See <https://github.com/mapnik/mapnik/wiki/LineSymbolizer> and the
-- 'LineSymbolizer' constructor fields for the available properties and their
-- meaning.
lineSym = LineSymbolizer
  { offset         = Nothing
  , lineRasterizer = Nothing
  , STROKE_PROPS_DEFS
  }
-- | See <https://github.com/mapnik/mapnik/wiki/LinePatternSymbolizer> and the
-- 'LinePatternSymbolizer' constructor fields for the available properties and their
-- meaning.
linePatternSym = LinePatternSymbolizer
  { file             = Nothing
  , opacity          = Nothing
  , offset           = Nothing
  , imageTransform   = Nothing
  , BASE_PROPS_DEFS
  }
-- | See <https://github.com/mapnik/mapnik/wiki/PolygonSymbolizer> and the
-- 'PolygonSymbolizer' constructor fields for the available properties and their
-- meaning.
polygonSym = PolygonSymbolizer
  { fill         = Nothing
  , fillOpacity  = Nothing
  , gamma        = Nothing
  , gammaMethod  = Nothing
  , BASE_PROPS_DEFS
  }
-- | See <https://github.com/mapnik/mapnik/wiki/PolygonPatternSymbolizer> and
-- the 'PolygonPatternSymbolizer' constructor fields for the available properties
-- and their meaning.
polygonPatternSym = PolygonPatternSymbolizer
  { file            = Nothing
  , opacity         = Nothing
  , gamma           = Nothing
  , gammaMethod     = Nothing
  , imageTransform  = Nothing
  , alignment       = Nothing
  , BASE_PROPS_DEFS
  }
-- | See <https://github.com/mapnik/mapnik/wiki/RasterSymbolizer> and the
-- 'RasterSymbolizer' constructor fields for the available properties
-- and their meaning.
rasterSym = RasterSymbolizer
  { scaling       = Nothing
  , rasterOpacity = Nothing
  , filterFactor  = Nothing
  , meshSize      = Nothing
  , preMultiplied = Nothing
  , colorizer     = Nothing
  , BASE_PROPS_DEFS
  }
-- | See <https://github.com/mapnik/mapnik/wiki/ShieldSymbolizer> and the
-- 'ShieldSymbolizer' constructor fields for the available properties
-- and their meaning.
shieldSym = ShieldSymbolizer
  { placements      = def
  , imageTransform  = Nothing
  , dx              = Nothing
  , dy              = Nothing
  , opacity         = Nothing
  , unlockImage     = Nothing
  , file            = Nothing
  , haloRasterizer  = Nothing
  , BASE_PROPS_DEFS
  }
-- | See <https://github.com/mapnik/mapnik/wiki/TextSymbolizer> and the
-- 'TextSymbolizer' constructor fields for the available properties
-- and their meaning.
textSym = TextSymbolizer
  { placements     = def
  , haloCompOp     = Nothing
  , haloRasterizer = Nothing
  , haloTransform  = Nothing
  , BASE_PROPS_DEFS
  }
-- | See <https://github.com/mapnik/mapnik/wiki/BuildingSymbolizer> and the
-- 'BuildingSymbolizer' constructor fields for the available properties
-- and their meaning.
buildingSym = BuildingSymbolizer
  { fill        = Nothing
  , fillOpacity = Nothing
  , height      = Nothing
  , BASE_PROPS_DEFS
  }
-- | See <https://github.com/mapnik/mapnik/wiki/MarkersSymbolizer> and the
-- 'MarkersSymbolizer' constructor fields for the available properties
-- and their meaning.
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
-- | See <https://github.com/mapnik/mapnik/wiki/GroupSymbolizer> and the
-- 'GroupSymbolizer' constructor fields for the available properties
-- and their meaning.
groupSym = GroupSymbolizer
  { groupProperties = def
  , numColumns      = Nothing
  , startColumn     = Nothing
  , repeatKey       = Nothing
  , placements      = def
  , BASE_PROPS_DEFS
  }
-- | See <https://github.com/mapnik/mapnik/wiki/DebugSymbolizer> and the
-- 'DebugSymbolizer' constructor fields for the available properties
-- and their meaning.
debugSym = DebugSymbolizer
  { mode = Nothing
  , BASE_PROPS_DEFS
  }
-- | See <https://github.com/mapnik/mapnik/wiki/DotSymbolizer> and the
-- 'DotSymbolizer' constructor fields for the available properties
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
deriveMapnikJSON ''Font
deriveMapnikJSON ''Stop
deriveMapnikJSON ''Colorizer
deriveMapnikJSON ''GroupLayout
deriveMapnikJSON ''GroupRule
deriveMapnikJSON ''SimplePlacementPosition
