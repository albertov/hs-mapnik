{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Symbolizer.TextProperties where

import Mapnik.Imports
import Mapnik.Enums
import Mapnik.Common
import Mapnik.Symbolizer.Property

import Data.Text (Text)
import Data.Default (Default)

import GHC.Generics (Generic)

data TextProperties = TextProperties
  { _textPropertiesLabelPlacement          :: !(PropValue LabelPlacement)
  , _textPropertiesLabelSpacing            :: !(PropValue Double)
  , _textPropertiesLabelPositionTolerance  :: !(PropValue Double)
  , _textPropertiesAvoidEdges              :: !(PropValue Bool)
  , _textPropertiesMargin                  :: !(PropValue Double)
  , _textPropertiesRepeatDistance          :: !(PropValue Double)
  , _textPropertiesMinimumDistance         :: !(PropValue Double) -- Deprecated
  , _textPropertiesMinimumPadding          :: !(PropValue Double)
  , _textPropertiesMinimumPathLength       :: !(PropValue Double)
  , _textPropertiesMaxCharAngleDelta       :: !(PropValue Double)
  , _textPropertiesAllowOverlap            :: !(PropValue Bool)
  , _textPropertiesLargestBoxOnly          :: !(PropValue Bool)
  , _textPropertiesUpright                 :: !(PropValue Upright)
  } deriving (Eq, Show, Generic, Default)
makeFields ''TextProperties
$(deriveMapnikJSON (length "_textProperties") ''TextProperties)

type FontSet = ()


data FormatProperties = FormatProperties
  { _formatPropertiesFaceName         :: !(PropValue Text)
  , _formatPropertiesFontSet          :: !(PropValue FontSet)
  , _formatPropertiesTextSize         :: !(PropValue Double)
  , _formatPropertiesCharacterSpacing :: !(PropValue Double)
  , _formatPropertiesLineSpacing      :: !(PropValue Double)
  , _formatPropertiesTextOpacity      :: !(PropValue Double)
  , _formatPropertiesHaloOpacity      :: !(PropValue Double)
  , _formatPropertiesTextTransform    :: !(PropValue TextTransform)
  , _formatPropertiesFill             :: !(PropValue Color)
  , _formatPropertiesHaloFill         :: !(PropValue Color)
  , _formatPropertiesHaloRadius       :: !(PropValue Double)
  , _formatPropertiesFfSettings       :: !(PropValue FontFeatureSettings)
  } deriving (Eq, Show, Generic, Default)
makeFields ''FormatProperties
$(deriveMapnikJSON (length "_formatProperties") ''FormatProperties)

data TextLayoutProperties = TextLayoutProperties
  { _textLayoutPropertiesDx                  :: !(PropValue Double)
  , _textLayoutPropertiesDy                  :: !(PropValue Double)
  , _textLayoutPropertiesOrientation         :: !(PropValue Double)
  , _textLayoutPropertiesTextRatio           :: !(PropValue Double)
  , _textLayoutPropertiesWrapWidth           :: !(PropValue Double)
  , _textLayoutPropertiesWrapChar            :: !(PropValue Text)
  , _textLayoutPropertiesWrapBefore          :: !(PropValue Bool)
  , _textLayoutPropertiesRepeatWrapChar      :: !(PropValue Bool)
  , _textLayoutPropertiesRotateDisplacement  :: !(PropValue Double)
  , _textLayoutPropertiesHorizontalAlignment :: !(PropValue HorizontalAlignment)
  , _textLayoutPropertiesJustifyAlignment    :: !(PropValue JustifyAlignment)
  , _textLayoutPropertiesVerticalAlignment   :: !(PropValue VerticalAlignment)
  , _textLayoutPropertiesMaxCharAngleDelta   :: !(PropValue Double)
  , _textLayoutPropertiesAllowOverlap        :: !(PropValue Bool)
  , _textLayoutPropertiesLargestBoxOnly      :: !(PropValue Bool)
  , _textLayoutPropertiesUpright             :: !(PropValue Upright)
  , _textLayoutPropertiesDirection           :: !(PropValue PlacementDirection)
  } deriving (Eq, Show, Generic, Default)
makeFields ''TextLayoutProperties
$(deriveMapnikJSON (length "_textLayoutProperties") ''TextLayoutProperties)
