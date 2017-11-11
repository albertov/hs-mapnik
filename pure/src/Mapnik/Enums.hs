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
deriveMapnikJSON ''CompositeMode

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
deriveMapnikJSON ''AspectFixMode

data LineCap
  = ButtCap
  | SquareCap
  | RoundCap
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''LineCap

data LineJoin
  = MiterJoin
  | MiterRevertJoin
  | RoundJoin
  | BevelJoin
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''LineJoin

data LineRasterizer
  = RasterizerFull
  | RasterizerFast
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''LineRasterizer

data HaloRasterizer
  = HaloRasterizerFull
  | HaloRasterizerFast
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''HaloRasterizer

data PointPlacement
  = CentroidPointPlacement
  | InteriorPointPlacement
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''PointPlacement


data PatternAlignment
  = LocalAlignment
  | GlobalAlignment
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''PatternAlignment

data DebugMode
  = DebugModeCollision
  | DebugModeVertex
  | DebugModeRings
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''DebugMode

data MarkerPlacement
  = MarkerPointPlacement
  | MarkerInteriorPlacement
  | MarkerLinePlacement
  | MarkerVertexFirstPlacement
  | MarkerVertexLastPlacement
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''MarkerPlacement

data MarkerMultiPolicy
  = MarkerEachMulti
  | MarkerWholeMulti
  | MarkerLargestMulti
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''MarkerMultiPolicy

data TextTransform
  = None
  | Uppercase
  | Lowercase
  | Capitalize
  | Reverse
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''TextTransform

data LabelPlacement
  = PointPlacement
  | LinePlacement
  | VertexPlacement
  | InteriorPlacement
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''LabelPlacement

data VerticalAlignment
  = VTop
  | VMiddle
  | VBottom
  | VAuto
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''VerticalAlignment

data HorizontalAlignment
  = HLeft
  | HMiddle
  | HRight
  | HAuto
  | HAdjust
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''HorizontalAlignment

data JustifyAlignment
  = JLeft
  | JMiddle
  | JRight
  | JAuto
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''JustifyAlignment

data Upright
  = UprightAuto
  | UprightAutoDown
  | UprightLeft
  | UprightRight
  | UprightLeftOnly
  | UprightRightOnly
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''Upright

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
deriveMapnikJSON ''Direction

data PlacementDirection
  = PlacementNorth
  | PlacementEast
  | PlacementSouth
  | PlacementWest
  | PlacementNorthEast
  | PlacementSouthEast
  | PlacementNorthWest
  | PlacementSouthWest
  | PlacementExact
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''PlacementDirection

data GammaMethod
  = GammaPower
  | GammaLinear
  | GammaNone
  | GammaThreshold
  | GammaMultiply
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''GammaMethod

data RasterMode
  = RasterFoo
  | RasterBar
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''RasterMode

data SimplifyAlgorithm
  = RadialDistance
  | DouglasPeucker
  | VisvalingamWhyatt
  | ZhaoSaalfeld
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''SimplifyAlgorithm

data ScalingMethod
  = Near
  | Bilinear
  | Bicubic
  | Spline16
  | Spline36
  | Hanning
  | Hamming
  | Hermite
  | Kaiser
  | Quadric
  | Catrom
  | Gaussian
  | Bessel
  | Mitchell
  | Sinc
  | Lanczos
  | Blackman
  deriving (Eq, Show, Bounded, Enum, Generic)
deriveMapnikJSON ''ScalingMethod
