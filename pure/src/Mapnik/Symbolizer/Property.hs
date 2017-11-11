{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Mapnik.Symbolizer.Property where

import Mapnik.Imports
import Mapnik.Enums
import Mapnik.Common

import Data.Text (Text)
import Data.Default (Default)

data Prop a = Exp Expression
            | Val a
  deriving (Eq, Show, Functor, Generic)
deriveMapnikJSON ''Prop

type PropValue a = Maybe (Prop a)


data Stop = Stop
  { value :: !Double
  , color :: !Color
  , mode  :: !ColorizerMode
  , label :: !(Maybe Text)
  } deriving (Eq, Show, Generic)
deriveMapnikJSON ''Stop

data Colorizer = Colorizer
  { mode :: !ColorizerMode
  , color :: !Color
  , stops  :: ![Stop]
  } deriving (Eq, Show, Generic)
deriveMapnikJSON ''Colorizer

data GroupProperties = GroupProperties
  deriving (Eq, Show, Generic)
deriveMapnikJSON ''GroupProperties


data FontSet = FontSet
  deriving (Eq, Show, Generic)
deriveMapnikJSON ''FontSet


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
deriveMapnikJSON ''TextProperties

data FormatProperties = FormatProperties
  { faceName         :: !(PropValue Text)
  , fontSet          :: !(PropValue FontSet)
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
deriveMapnikJSON ''FormatProperties

data TextLayoutProperties = TextLayoutProperties
  { dx                  :: !(PropValue Double)
  , dy                  :: !(PropValue Double)
  , orientation         :: !(PropValue Double)
  , textRatio           :: !(PropValue Double)
  , wrapWidth           :: !(PropValue Double)
  , wrapChar            :: !(PropValue Text)
  , wrapBefore          :: !(PropValue Bool)
  , repeatWrapChar      :: !(PropValue Bool)
  , rotateDisplacement  :: !(PropValue Double)
  , horizontalAlignment :: !(PropValue HorizontalAlignment)
  , justifyAlignment    :: !(PropValue JustifyAlignment)
  , verticalAlignment   :: !(PropValue VerticalAlignment)
  , maxCharAngleDelta   :: !(PropValue Double)
  , allowOverlap        :: !(PropValue Bool)
  , largestBoxOnly      :: !(PropValue Bool)
  , upright             :: !(PropValue Upright)
  , direction           :: !(PropValue PlacementDirection)
  } deriving (Eq, Show, Generic, Default)
deriveMapnikJSON ''TextLayoutProperties

data SimplePlacementPosition = SimplePlacementPosition
  { textSizes            :: ![Int]
  , directions           :: ![PlacementDirection]
  } deriving (Eq, Show, Generic, Default)
deriveMapnikJSON ''SimplePlacementPosition

data TextPlacements
  = Simple
    { defaults  :: !TextProperties
    , positions :: !(PropValue [SimplePlacementPosition])
    }
  | List
    { defaults :: !TextProperties
    , placements :: ![TextProperties]
    }
  | Dummy
    { defaults :: !TextProperties
    }
  deriving (Eq, Show, Generic)
deriveMapnikJSON ''TextPlacements
