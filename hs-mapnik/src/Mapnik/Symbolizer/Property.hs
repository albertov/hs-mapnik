{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Mapnik.Symbolizer.Property (
  module Mapnik.Symbolizer.Property
, DSum(..)
, def
) where

import Mapnik.Imports
import Mapnik.Enums
import Mapnik.Common

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Default (Default(def))
import Data.Dependent.Sum as DSum (DSum(..), ShowTag(..), EqTag(..))
import qualified Data.Dependent.Map as DMap
import GHC.Exts (IsList(..))

--TODO
type FontFeatureSettings = ()
type SimplifyAlgorithm = ()
type GroupProperties = ()
type Scaling = ()
type TextPlacements = ()
type Colorizer = ()

data PropValue v = PropExpression Expression
                 | PropValue      v
                 | PropDefault
  deriving (Eq, Show, Functor)
deriveMapnikJSON ''PropValue

type Property = DSum Key PropValue
type Properties = DMap.DMap Key PropValue

instance IsList Properties where
  type Item Properties = Property
  fromList = DMap.fromList
  toList = DMap.toList


instance Default (PropValue a) where def = PropDefault

data Key a where
    Gamma :: Key Double
    GammaMethod :: Key GammaMethod
    Opacity :: Key Double
    Alignment :: Key PatternAlignment
    Offset :: Key Double
    CompOp :: Key CompositeMode
    Clip :: Key Bool
    Fill :: Key Color
    FillOpacity :: Key Double
    Stroke :: Key Color
    StrokeWidth :: Key Double
    StrokeOpacity :: Key Double
    StrokeLinejoin :: Key LineJoin
    StrokeLinecap :: Key LineCap
    StrokeGamma :: Key Double
    StrokeGammaMethod :: Key GammaMethod
    StrokeDashoffset :: Key Double
    StrokeDasharray :: Key DashArray
    StrokeMiterlimit :: Key Double
    GeometryTransform :: Key Transform
    LineRasterizer :: Key LineRasterizer
    ImageTransform :: Key Transform
    Spacing :: Key Double
    MaxError :: Key Double
    AllowOverlap :: Key Bool
    IgnorePlacement :: Key Bool
    Width :: Key Double
    Height :: Key Double
    File :: Key FilePath
    ShieldDx :: Key Double
    ShieldDy :: Key Double
    UnlockImage :: Key Bool
    Mode :: Key Text
    Scaling :: Key Scaling
    FilterFactor :: Key Double
    MeshSize :: Key Int
    Premultiplied :: Key Bool
    Smooth :: Key Double
    SimplifyAlgorithm :: Key SimplifyAlgorithm
    SimplifyTolerance :: Key Double
    HaloRasterizer :: Key HaloRasterizer
    TextPlacements :: Key TextPlacements
    LabelPlacement :: Key LabelPlacement
    MarkersPlacementType :: Key MarkerPlacement
    MarkersMultipolicy :: Key MarkerMultiPolicy
    PointPlacementType :: Key PointPlacement
    Colorizer :: Key Colorizer
    HaloTransform :: Key Transform
    NumColumns :: Key Int
    StartColumn :: Key Int
    RepeatKey :: Key Expression
    GroupProperties :: Key GroupProperties
    LargestBoxOnly :: Key Bool
    MinimumPathLength :: Key Double
    HaloCompOp :: Key CompositeMode
    TextTransform :: Key TextTransform
    HorizontalAlignment :: Key HorizontalAlignment
    JustifyAlignment :: Key JustifyAlignment
    VerticalAlignment :: Key VerticalAlignment
    Upright :: Key Upright
    Direction :: Key Direction
    AvoidEdges :: Key Bool
    FfSettings :: Key FontFeatureSettings

deriveGCompare ''Key
deriveGEq ''Key
deriveGShow ''Key
deriving instance Show (Key a)
deriving instance Eq (Key a)

(==>) :: Key v -> v -> Property
k ==> v = k :=> PropValue v

(=~>) :: Key v -> Expression -> Property
k =~> v = k :=> PropExpression v

instance ShowTag Key PropValue where
  showTaggedPrec Gamma = showsPrec
  showTaggedPrec GammaMethod = showsPrec
  showTaggedPrec Opacity = showsPrec
  showTaggedPrec Alignment = showsPrec
  showTaggedPrec Offset = showsPrec
  showTaggedPrec CompOp = showsPrec
  showTaggedPrec Clip = showsPrec
  showTaggedPrec Fill = showsPrec
  showTaggedPrec FillOpacity = showsPrec
  showTaggedPrec Stroke = showsPrec
  showTaggedPrec StrokeWidth = showsPrec
  showTaggedPrec StrokeOpacity = showsPrec
  showTaggedPrec StrokeLinejoin = showsPrec
  showTaggedPrec StrokeLinecap = showsPrec
  showTaggedPrec StrokeGamma = showsPrec
  showTaggedPrec StrokeGammaMethod = showsPrec
  showTaggedPrec StrokeDashoffset = showsPrec
  showTaggedPrec StrokeDasharray = showsPrec
  showTaggedPrec StrokeMiterlimit = showsPrec
  showTaggedPrec GeometryTransform = showsPrec
  showTaggedPrec LineRasterizer = showsPrec
  showTaggedPrec ImageTransform = showsPrec
  showTaggedPrec Spacing = showsPrec
  showTaggedPrec MaxError = showsPrec
  showTaggedPrec AllowOverlap = showsPrec
  showTaggedPrec IgnorePlacement = showsPrec
  showTaggedPrec Width = showsPrec
  showTaggedPrec Height = showsPrec
  showTaggedPrec File = showsPrec
  showTaggedPrec ShieldDx = showsPrec
  showTaggedPrec ShieldDy = showsPrec
  showTaggedPrec UnlockImage = showsPrec
  showTaggedPrec Mode = showsPrec
  showTaggedPrec Scaling = showsPrec
  showTaggedPrec FilterFactor = showsPrec
  showTaggedPrec MeshSize = showsPrec
  showTaggedPrec Premultiplied = showsPrec
  showTaggedPrec Smooth = showsPrec
  showTaggedPrec SimplifyAlgorithm = showsPrec
  showTaggedPrec SimplifyTolerance = showsPrec
  showTaggedPrec HaloRasterizer = showsPrec
  showTaggedPrec TextPlacements = showsPrec
  showTaggedPrec LabelPlacement = showsPrec
  showTaggedPrec MarkersPlacementType = showsPrec
  showTaggedPrec MarkersMultipolicy = showsPrec
  showTaggedPrec PointPlacementType = showsPrec
  showTaggedPrec Colorizer = showsPrec
  showTaggedPrec HaloTransform = showsPrec
  showTaggedPrec NumColumns = showsPrec
  showTaggedPrec StartColumn = showsPrec
  showTaggedPrec RepeatKey = showsPrec
  showTaggedPrec GroupProperties = showsPrec
  showTaggedPrec LargestBoxOnly = showsPrec
  showTaggedPrec MinimumPathLength = showsPrec
  showTaggedPrec HaloCompOp = showsPrec
  showTaggedPrec TextTransform = showsPrec
  showTaggedPrec HorizontalAlignment = showsPrec
  showTaggedPrec JustifyAlignment = showsPrec
  showTaggedPrec VerticalAlignment = showsPrec
  showTaggedPrec Upright = showsPrec
  showTaggedPrec Direction = showsPrec
  showTaggedPrec AvoidEdges = showsPrec
  showTaggedPrec FfSettings = showsPrec

instance EqTag Key PropValue where
  eqTagged Gamma Gamma = (==)
  eqTagged GammaMethod GammaMethod = (==)
  eqTagged Opacity Opacity = (==)
  eqTagged Alignment Alignment = (==)
  eqTagged Offset Offset = (==)
  eqTagged CompOp CompOp = (==)
  eqTagged Clip Clip = (==)
  eqTagged Fill Fill = (==)
  eqTagged FillOpacity FillOpacity = (==)
  eqTagged Stroke Stroke = (==)
  eqTagged StrokeWidth StrokeWidth = (==)
  eqTagged StrokeOpacity StrokeOpacity = (==)
  eqTagged StrokeLinejoin StrokeLinejoin = (==)
  eqTagged StrokeLinecap StrokeLinecap = (==)
  eqTagged StrokeGamma StrokeGamma = (==)
  eqTagged StrokeGammaMethod StrokeGammaMethod = (==)
  eqTagged StrokeDashoffset StrokeDashoffset = (==)
  eqTagged StrokeDasharray StrokeDasharray = (==)
  eqTagged StrokeMiterlimit StrokeMiterlimit = (==)
  eqTagged GeometryTransform GeometryTransform = (==)
  eqTagged LineRasterizer LineRasterizer = (==)
  eqTagged ImageTransform ImageTransform = (==)
  eqTagged Spacing Spacing = (==)
  eqTagged MaxError MaxError = (==)
  eqTagged AllowOverlap AllowOverlap = (==)
  eqTagged IgnorePlacement IgnorePlacement = (==)
  eqTagged Width Width = (==)
  eqTagged Height Height = (==)
  eqTagged File File = (==)
  eqTagged ShieldDx ShieldDx = (==)
  eqTagged ShieldDy ShieldDy = (==)
  eqTagged UnlockImage UnlockImage = (==)
  eqTagged Mode Mode = (==)
  eqTagged Scaling Scaling = (==)
  eqTagged FilterFactor FilterFactor = (==)
  eqTagged MeshSize MeshSize = (==)
  eqTagged Premultiplied Premultiplied = (==)
  eqTagged Smooth Smooth = (==)
  eqTagged SimplifyAlgorithm SimplifyAlgorithm = (==)
  eqTagged SimplifyTolerance SimplifyTolerance = (==)
  eqTagged HaloRasterizer HaloRasterizer = (==)
  eqTagged TextPlacements TextPlacements = (==)
  eqTagged LabelPlacement LabelPlacement = (==)
  eqTagged MarkersPlacementType MarkersPlacementType = (==)
  eqTagged MarkersMultipolicy MarkersMultipolicy = (==)
  eqTagged PointPlacementType PointPlacementType = (==)
  eqTagged Colorizer Colorizer = (==)
  eqTagged HaloTransform HaloTransform = (==)
  eqTagged NumColumns NumColumns = (==)
  eqTagged StartColumn StartColumn = (==)
  eqTagged RepeatKey RepeatKey = (==)
  eqTagged GroupProperties GroupProperties = (==)
  eqTagged LargestBoxOnly LargestBoxOnly = (==)
  eqTagged MinimumPathLength MinimumPathLength = (==)
  eqTagged HaloCompOp HaloCompOp = (==)
  eqTagged TextTransform TextTransform = (==)
  eqTagged HorizontalAlignment HorizontalAlignment = (==)
  eqTagged JustifyAlignment JustifyAlignment = (==)
  eqTagged VerticalAlignment VerticalAlignment = (==)
  eqTagged Upright Upright = (==)
  eqTagged Direction Direction = (==)
  eqTagged AvoidEdges AvoidEdges = (==)
  eqTagged FfSettings FfSettings = (==)
  eqTagged _ _ = \_ _ -> False

parseProperties :: Object -> Parser Properties
parseProperties =  fmap DMap.fromList . mapM (uncurry parseProperty) . HM.toList 

parseProperty :: Text -> Value -> Parser Property
parseProperty "gamma" v = (Gamma :=>) <$> parseJSON v
parseProperty "gammaMethod" v = (GammaMethod :=>) <$> parseJSON v
parseProperty "opacity" v = (Opacity :=>) <$> parseJSON v
parseProperty "alignment" v = (Alignment :=>) <$> parseJSON v
parseProperty "offset" v = (Offset :=>) <$> parseJSON v
parseProperty "compOp" v = (CompOp :=>) <$> parseJSON v
parseProperty "clip" v = (Clip :=>) <$> parseJSON v
parseProperty "fill" v = (Fill :=>) <$> parseJSON v
parseProperty "fillOpacity" v = (FillOpacity :=>) <$> parseJSON v
parseProperty "stroke" v = (Stroke :=>) <$> parseJSON v
parseProperty "strokeWidth" v = (StrokeWidth :=>) <$> parseJSON v
parseProperty "strokeOpacity" v = (StrokeOpacity :=>) <$> parseJSON v
parseProperty "strokeLinejoin" v = (StrokeLinejoin :=>) <$> parseJSON v
parseProperty "strokeLinecap" v = (StrokeLinecap :=>) <$> parseJSON v
parseProperty "strokeGamma" v = (StrokeGamma :=>) <$> parseJSON v
parseProperty "strokeGammaMethod" v = (StrokeGammaMethod :=>) <$> parseJSON v
parseProperty "strokeDashoffset" v = (StrokeDashoffset :=>) <$> parseJSON v
parseProperty "strokeDasharray" v = (StrokeDasharray :=>) <$> parseJSON v
parseProperty "strokeMiterlimit" v = (StrokeMiterlimit :=>) <$> parseJSON v
parseProperty "geometryTransform" v = (GeometryTransform :=>) <$> parseJSON v
parseProperty "lineRasterizer" v = (LineRasterizer :=>) <$> parseJSON v
parseProperty "imageTransform" v = (ImageTransform :=>) <$> parseJSON v
parseProperty "spacing" v = (Spacing :=>) <$> parseJSON v
parseProperty "maxError" v = (MaxError :=>) <$> parseJSON v
parseProperty "allowOverlap" v = (AllowOverlap :=>) <$> parseJSON v
parseProperty "ignorePlacement" v = (IgnorePlacement :=>) <$> parseJSON v
parseProperty "width" v = (Width :=>) <$> parseJSON v
parseProperty "height" v = (Height :=>) <$> parseJSON v
parseProperty "file" v = (File :=>) <$> parseJSON v
parseProperty "shieldDx" v = (ShieldDx :=>) <$> parseJSON v
parseProperty "shieldDy" v = (ShieldDy :=>) <$> parseJSON v
parseProperty "unlockImage" v = (UnlockImage :=>) <$> parseJSON v
parseProperty "mode" v = (Mode :=>) <$> parseJSON v
parseProperty "scaling" v = (Scaling :=>) <$> parseJSON v
parseProperty "filterFactor" v = (FilterFactor :=>) <$> parseJSON v
parseProperty "meshSize" v = (MeshSize :=>) <$> parseJSON v
parseProperty "premultiplied" v = (Premultiplied :=>) <$> parseJSON v
parseProperty "smooth" v = (Smooth :=>) <$> parseJSON v
parseProperty "simplifyAlgorithm" v = (SimplifyAlgorithm :=>) <$> parseJSON v
parseProperty "simplifyTolerance" v = (SimplifyTolerance :=>) <$> parseJSON v
parseProperty "haloRasterizer" v = (HaloRasterizer :=>) <$> parseJSON v
parseProperty "placements" v = (TextPlacements :=>) <$> parseJSON v
parseProperty "labelPlacement" v = (LabelPlacement :=>) <$> parseJSON v
parseProperty "markersPlacementType" v = (MarkersPlacementType :=>) <$> parseJSON v
parseProperty "markersMultipolicy" v = (MarkersMultipolicy :=>) <$> parseJSON v
parseProperty "pointPlacementType" v = (PointPlacementType :=>) <$> parseJSON v
parseProperty "colorizer" v = (Colorizer :=>) <$> parseJSON v
parseProperty "haloTransform" v = (HaloTransform :=>) <$> parseJSON v
parseProperty "numColumns" v = (NumColumns :=>) <$> parseJSON v
parseProperty "startColumn" v = (StartColumn :=>) <$> parseJSON v
parseProperty "repeatKey" v = (RepeatKey :=>) <$> parseJSON v
parseProperty "groupProperties" v = (GroupProperties :=>) <$> parseJSON v
parseProperty "largestBoxOnly" v = (LargestBoxOnly :=>) <$> parseJSON v
parseProperty "minimumPathLength" v = (MinimumPathLength :=>) <$> parseJSON v
parseProperty "haloCompOp" v = (HaloCompOp :=>) <$> parseJSON v
parseProperty "textTransform" v = (TextTransform :=>) <$> parseJSON v
parseProperty "horizontalAlignment" v = (HorizontalAlignment :=>) <$> parseJSON v
parseProperty "justifyAlignment" v = (JustifyAlignment :=>) <$> parseJSON v
parseProperty "verticalAlignment" v = (VerticalAlignment :=>) <$> parseJSON v
parseProperty "upright" v = (Upright :=>) <$> parseJSON v
parseProperty "direction" v = (Direction :=>) <$> parseJSON v
parseProperty "avoidEdges" v = (AvoidEdges :=>) <$> parseJSON v
parseProperty "ffSettings" v = (FfSettings :=>) <$> parseJSON v
parseProperty _ _ = fail "Unknown symbolizer property"

serializeProperty :: Property -> (Text,Value)
serializeProperty (Gamma :=> v) = ("gamma", toJSON v)
serializeProperty (GammaMethod :=> v) = ("gammaMethod", toJSON v)
serializeProperty (Opacity :=> v) = ("opacity", toJSON v)
serializeProperty (Alignment :=> v) = ("alignment", toJSON v)
serializeProperty (Offset :=> v) = ("offset", toJSON v)
serializeProperty (CompOp :=> v) = ("compOp", toJSON v)
serializeProperty (Clip :=> v) = ("clip", toJSON v)
serializeProperty (Fill :=> v) = ("fill", toJSON v)
serializeProperty (FillOpacity :=> v) = ("fillOpacity", toJSON v)
serializeProperty (Stroke :=> v) = ("stroke", toJSON v)
serializeProperty (StrokeWidth :=> v) = ("strokeWidth", toJSON v)
serializeProperty (StrokeOpacity :=> v) = ("strokeOpacity", toJSON v)
serializeProperty (StrokeLinejoin :=> v) = ("strokeLinejoin", toJSON v)
serializeProperty (StrokeLinecap :=> v) = ("strokeLinecap", toJSON v)
serializeProperty (StrokeGamma :=> v) = ("strokeGamma", toJSON v)
serializeProperty (StrokeGammaMethod :=> v) = ("strokeGammaMethod", toJSON v)
serializeProperty (StrokeDashoffset :=> v) = ("strokeDashoffset", toJSON v)
serializeProperty (StrokeDasharray :=> v) = ("strokeDasharray", toJSON v)
serializeProperty (StrokeMiterlimit :=> v) = ("strokeMiterlimit", toJSON v)
serializeProperty (GeometryTransform :=> v) = ("geometryTransform", toJSON v)
serializeProperty (LineRasterizer :=> v) = ("lineRasterizer", toJSON v)
serializeProperty (ImageTransform :=> v) = ("imageTransform", toJSON v)
serializeProperty (Spacing :=> v) = ("spacing", toJSON v)
serializeProperty (MaxError :=> v) = ("maxError", toJSON v)
serializeProperty (AllowOverlap :=> v) = ("allowOverlap", toJSON v)
serializeProperty (IgnorePlacement :=> v) = ("ignorePlacement", toJSON v)
serializeProperty (Width :=> v) = ("width", toJSON v)
serializeProperty (Height :=> v) = ("height", toJSON v)
serializeProperty (File :=> v) = ("file", toJSON v)
serializeProperty (ShieldDx :=> v) = ("shieldDx", toJSON v)
serializeProperty (ShieldDy :=> v) = ("shieldDy", toJSON v)
serializeProperty (UnlockImage :=> v) = ("unlockImage", toJSON v)
serializeProperty (Mode :=> v) = ("mode", toJSON v)
serializeProperty (Scaling :=> v) = ("scaling", toJSON v)
serializeProperty (FilterFactor :=> v) = ("filterFactor", toJSON v)
serializeProperty (MeshSize :=> v) = ("meshSize", toJSON v)
serializeProperty (Premultiplied :=> v) = ("premultiplied", toJSON v)
serializeProperty (Smooth :=> v) = ("smooth", toJSON v)
serializeProperty (SimplifyAlgorithm :=> v) = ("simplifyAlgorithm", toJSON v)
serializeProperty (SimplifyTolerance :=> v) = ("simplifyTolerance", toJSON v)
serializeProperty (HaloRasterizer :=> v) = ("haloRasterizer", toJSON v)
serializeProperty (TextPlacements :=> v) = ("placements", toJSON v)
serializeProperty (LabelPlacement :=> v) = ("labelPlacement", toJSON v)
serializeProperty (MarkersPlacementType :=> v) = ("markersPlacementType", toJSON v)
serializeProperty (MarkersMultipolicy :=> v) = ("markersMultipolicy", toJSON v)
serializeProperty (PointPlacementType :=> v) = ("pointPlacementType", toJSON v)
serializeProperty (Colorizer :=> v) = ("colorizer", toJSON v)
serializeProperty (HaloTransform :=> v) = ("haloTransform", toJSON v)
serializeProperty (NumColumns :=> v) = ("numColumns", toJSON v)
serializeProperty (StartColumn :=> v) = ("startColumn", toJSON v)
serializeProperty (RepeatKey :=> v) = ("repeatKey", toJSON v)
serializeProperty (GroupProperties :=> v) = ("groupProperties", toJSON v)
serializeProperty (LargestBoxOnly :=> v) = ("largestBoxOnly", toJSON v)
serializeProperty (MinimumPathLength :=> v) = ("minimumPathLength", toJSON v)
serializeProperty (HaloCompOp :=> v) = ("haloCompOp", toJSON v)
serializeProperty (TextTransform :=> v) = ("textTransform", toJSON v)
serializeProperty (HorizontalAlignment :=> v) = ("horizontalAlignment", toJSON v)
serializeProperty (JustifyAlignment :=> v) = ("justifyAlignment", toJSON v)
serializeProperty (VerticalAlignment :=> v) = ("verticalAlignment", toJSON v)
serializeProperty (Upright :=> v) = ("upright", toJSON v)
serializeProperty (Direction :=> v) = ("direction", toJSON v)
serializeProperty (AvoidEdges :=> v) = ("avoidEdges", toJSON v)
serializeProperty (FfSettings :=> v) = ("ffSettings", toJSON v)
