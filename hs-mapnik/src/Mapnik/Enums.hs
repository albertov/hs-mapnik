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

data LineCap
  = ButtCap
  | SquareCap
  | RoundCap
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON LineCap where toEncoding = mapnikJsonEncoder 0
instance FromJSON LineCap where parseJSON = mapnikJsonDecoder 0

data LineJoin
  = MiterJoin
  | MiterRevertJoin
  | RoundJoin
  | BevelJoin
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON LineJoin where toEncoding = mapnikJsonEncoder 0
instance FromJSON LineJoin where parseJSON = mapnikJsonDecoder 0

data LineRasterizer
  = RasterizerFull
  | RasterizerFast
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON LineRasterizer where toEncoding = mapnikJsonEncoder 0
instance FromJSON LineRasterizer where parseJSON = mapnikJsonDecoder 0

data HaloRasterizer
  = HaloRasterizerFull
  | HaloRasterizerFast
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON HaloRasterizer where toEncoding = mapnikJsonEncoder 0
instance FromJSON HaloRasterizer where parseJSON = mapnikJsonDecoder 0

data PointPlacement
  = CentroidPointPlacement
  | InteriorPointPlacement
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON PointPlacement where toEncoding = mapnikJsonEncoder 0
instance FromJSON PointPlacement where parseJSON = mapnikJsonDecoder 0


data PatternAlignment
  = LocalAlignment
  | GlobalAlignment
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON PatternAlignment where toEncoding = mapnikJsonEncoder 0
instance FromJSON PatternAlignment where parseJSON = mapnikJsonDecoder 0

data DebugMode
  = DebugModeCollision
  | DebugModeVertex
  | DebugModeRings
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON DebugMode where toEncoding = mapnikJsonEncoder 0
instance FromJSON DebugMode where parseJSON = mapnikJsonDecoder 0

data MarkerPlacement
  = MarkerPointPlacement
  | MarkerInteriorPlacement
  | MarkerLinePlacement
  | MarkerVertexFirstPlacement
  | MarkerVertexLastPlacement
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON MarkerPlacement where toEncoding = mapnikJsonEncoder 0
instance FromJSON MarkerPlacement where parseJSON = mapnikJsonDecoder 0

data MarkerMultiPolicy
  = MarkerEachMulti
  | MarkerWholeMulti
  | MarkerLargestMulti
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON MarkerMultiPolicy where toEncoding = mapnikJsonEncoder 0
instance FromJSON MarkerMultiPolicy where parseJSON = mapnikJsonDecoder 0

data TextTransform
  = None
  | Uppercase
  | Lowercase
  | Capitalize
  | Reverse
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON TextTransform where toEncoding = mapnikJsonEncoder 0
instance FromJSON TextTransform where parseJSON = mapnikJsonDecoder 0

data LabelPlacement
  = PointPlacement
  | LinePlacement
  | VertexPlacement
  | InteriorPlacement
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON LabelPlacement where toEncoding = mapnikJsonEncoder 0
instance FromJSON LabelPlacement where parseJSON = mapnikJsonDecoder 0

data VerticalAlignment
  = VTop
  | VMiddle
  | VBottom
  | VAuto
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON VerticalAlignment where toEncoding = mapnikJsonEncoder 0
instance FromJSON VerticalAlignment where parseJSON = mapnikJsonDecoder 0

data HorizontalAlignment
  = HLeft
  | HMiddle
  | HRight
  | HAuto
  | HAdjust
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON HorizontalAlignment where toEncoding = mapnikJsonEncoder 0
instance FromJSON HorizontalAlignment where parseJSON = mapnikJsonDecoder 0

data JustifyAlignment
  = JLeft
  | JMiddle
  | JRight
  | JAuto
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON JustifyAlignment where toEncoding = mapnikJsonEncoder 0
instance FromJSON JustifyAlignment where parseJSON = mapnikJsonDecoder 0

data Upright
  = UprightAuto
  | UprightAutoDown
  | UprightLeft
  | UprightRight
  | UprightLeftOnly
  | UprightRightOnly
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON Upright where toEncoding = mapnikJsonEncoder 0
instance FromJSON Upright where parseJSON = mapnikJsonDecoder 0

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
instance ToJSON Direction where toEncoding = mapnikJsonEncoder 0
instance FromJSON Direction where parseJSON = mapnikJsonDecoder 0

data GammaMethod
  = GammaPower
  | GammaLinear
  | GammaNone
  | GammaThreshold
  | GammaMultiply
  deriving (Eq, Show, Bounded, Enum, Generic)
instance ToJSON GammaMethod where toEncoding = mapnikJsonEncoder 0
instance FromJSON GammaMethod where parseJSON = mapnikJsonDecoder 0
