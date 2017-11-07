{-# LANGUAGE DeriveGeneric #-}
module Mapnik.Enums where

import Mapnik.Imports

-- WARNING: These *must* be kept in sync with mapnik headers' since
-- we use to/fromEnum and static_casts for conversion!!

data CompositeMode
  = Clear
  | Src
  | Dst
  | SrcOver
  | DstOver
  | SrcIn
  | DstIn
  | SrcOut
  | DstOut
  | SrcAtop
  | DstAtop
  | Xor
  | Plus
  | Minus
  | Multiply
  | Screen
  | Overlay
  | Darken
  | Lighten
  | ColorDodge
  | ColorBurn
  | HardLight
  | SoftLight
  | Difference
  | Exclusion
  | Contrast
  | Invert
  | InvertRgb
  | GrainMerge
  | GrainExtract
  | Hue
  | Saturation
  | Color
  | Value
  | LinearDodge
  | LinearBurn
  | Divide
  deriving (Eq, Show, Enum, Bounded, Generic)

instance ToJSON CompositeMode where toEncoding = mapnikJsonEncoder 0
instance FromJSON CompositeMode where parseJSON = mapnikJsonDecoder 0

data AspectFixMode
  = GrowBox
  | GrowCanvas
  | ShrinkBox
  | ShrinkCanvas
  | AdjustBoxWidth
  | AdjustBoxHeight
  | AdjustCanvasWidth
  | AdjustCanvasHeight
  | Respect
  deriving (Eq, Show, Bounded, Enum, Generic)

instance ToJSON AspectFixMode where toEncoding = mapnikJsonEncoder 0
instance FromJSON AspectFixMode where parseJSON = mapnikJsonDecoder 0
