{-# LANGUAGE TemplateHaskell #-}
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
deriveMapnikJSON 0 ''CompositeMode

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
deriveMapnikJSON 0 ''AspectFixMode

data LineCap
  = ButtCap
  | SquareCap
  | RoundCap
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''LineCap

data LineJoin
  = MiterJoin
  | MiterRevertJoin
  | RoundJoin
  | BevelJoin
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''LineJoin

data LineRasterizer
  = RasterizerFull
  | RasterizerFast
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''LineRasterizer

data HaloRasterizer
  = HaloRasterizerFull
  | HaloRasterizerFast
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''HaloRasterizer

data PointPlacement
  = CentroidPointPlacement
  | InteriorPointPlacement
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''PointPlacement


data PatternAlignment
  = LocalAlignment
  | GlobalAlignment
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''PatternAlignment

data DebugMode
  = DebugModeCollision
  | DebugModeVertex
  | DebugModeRings
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''DebugMode

data MarkerPlacement
  = MarkerPointPlacement
  | MarkerInteriorPlacement
  | MarkerLinePlacement
  | MarkerVertexFirstPlacement
  | MarkerVertexLastPlacement
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''MarkerPlacement

data MarkerMultiPolicy
  = MarkerEachMulti
  | MarkerWholeMulti
  | MarkerLargestMulti
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''MarkerMultiPolicy

data TextTransform
  = None
  | Uppercase
  | Lowercase
  | Capitalize
  | Reverse
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''TextTransform

data LabelPlacement
  = PointPlacement
  | LinePlacement
  | VertexPlacement
  | InteriorPlacement
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''LabelPlacement

data VerticalAlignment
  = VTop
  | VMiddle
  | VBottom
  | VAuto
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''VerticalAlignment

data HorizontalAlignment
  = HLeft
  | HMiddle
  | HRight
  | HAuto
  | HAdjust
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''HorizontalAlignment

data JustifyAlignment
  = JLeft
  | JMiddle
  | JRight
  | JAuto
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''JustifyAlignment

data Upright
  = UprightAuto
  | UprightAutoDown
  | UprightLeft
  | UprightRight
  | UprightLeftOnly
  | UprightRightOnly
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''Upright

data Direction
  = DirectionLeft
  | DirectionRight
  | DirectionLeftOnly
  | DirectionRightOnly
  | DirectionAuto
  | DirectionAutoDown
  | DirectionUp
  | DirectionDown
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''Direction

data GammaMethod
  = GammaPower
  | GammaLinear
  | GammaNone
  | GammaThreshold
  | GammaMultiply
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON 0 ''GammaMethod
