{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Mapnik.Symbolizer.Property (
  module Mapnik.Symbolizer.Property
, def
) where

import Mapnik.Imports
import Mapnik.Enums
import Mapnik.Common

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

data TextFormatProperties = TextFormatProperties
  { faceName         :: !(Maybe Text)
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
deriveMapnikJSON ''TextFormatProperties

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
  , direction           :: !(Maybe PlacementDirection)
  } deriving (Eq, Show, Generic, Default)
deriveMapnikJSON ''TextLayoutProperties

data Format
  = FormatExp    !Expression
  | FormatList   ![Format]
  | Format
    { faceName         :: !(Maybe Text)
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
    , wrapChar            :: !(PropValue Text)
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
deriveMapnikJSON ''Format

instance Default Format where def = NullFormat

data TextSymProperties = TextSymProperties
  { properties       :: !TextProperties
  , layoutProperties :: !TextLayoutProperties
  , formatProperties :: !TextFormatProperties
  , format           :: !Format
  } deriving (Eq, Show, Generic, Default)
deriveMapnikJSON ''TextSymProperties


newtype TextPlacements = Dummy TextSymProperties
  deriving (Generic)
  deriving newtype (Eq, Show, Default)
deriveMapnikJSON ''TextPlacements
