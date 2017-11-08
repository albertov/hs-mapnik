{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Mapnik.Symbolizer.TextProperties where

import Mapnik.Imports
import Mapnik.Enums
import Mapnik.Common
import Mapnik.Symbolizer.Property

import Data.Text (Text)
import Data.Default (Default)

import GHC.Generics (Generic)

type FontSet = ()



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
