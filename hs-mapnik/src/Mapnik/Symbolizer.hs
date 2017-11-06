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
module Mapnik.Symbolizer (
  module Mapnik.Symbolizer
, DSum (..)
) where

import Mapnik.Imports
import Mapnik.Expression
import Control.Applicative
import Data.Functor.Identity (Identity)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Dependent.Sum as DSum (DSum(..), ShowTag(..), EqTag(..))
import qualified Data.Dependent.Map as DMap
import Data.String (fromString)

data PropValue v = PropValue      v
                 | PropExpression Expression
  deriving (Eq, Show, Functor)

instance ToJSON v => ToJSON (PropValue v) where
  toJSON (PropValue v)      = toJSON v
  toJSON (PropExpression v) = toJSON v

instance FromJSON v => FromJSON (PropValue v) where
  parseJSON o = (PropValue <$> parseJSON o) <|> (PropExpression <$> parseJSON o)

type Property = DSum Key PropValue

type Properties = DMap.DMap Key PropValue

data Key a where
    Gamma :: Key Double
    GammaMethod :: Key ()
    Opacity :: Key Double
    Alignment :: Key ()
    Offset :: Key Double
    CompOp :: Key ()
    Clip :: Key Bool
    Fill :: Key Color
    FillOpacity :: Key Double
    Stroke :: Key Color
    StrokeWidth :: Key Double
    StrokeOpacity :: Key Double
    StrokeLinejoin :: Key ()
    StrokeLinecap :: Key ()
    StrokeGamma :: Key Double
    StrokeGammaMethod :: Key ()
    StrokeDashoffset :: Key Double
    StrokeDasharray :: Key ()
    StrokeMiterlimit :: Key Double
    GeometryTransform :: Key ()
    LineRasterizer :: Key ()
    ImageTransform :: Key ()
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
    Scaling :: Key ()
    FilterFactor :: Key Double
    MeshSize :: Key Int
    Premultiplied :: Key Bool
    Smooth :: Key Double
    SimplifyAlgorithm :: Key ()
    SimplifyTolerance :: Key Double
    HaloRasterizer :: Key ()
    TextPlacements_ :: Key ()
    LabelPlacement :: Key ()
    MarkersPlacementType :: Key ()
    MarkersMultipolicy :: Key ()
    PointPlacementType :: Key ()
    Colorizer :: Key ()
    HaloTransform :: Key ()
    NumColumns :: Key Int
    StartColumn :: Key Int
    RepeatKey :: Key ()
    GroupProperties :: Key ()
    LargestBoxOnly :: Key ()
    MinimumPathLength :: Key Double
    HaloCompOp :: Key ()
    TextTransform :: Key ()
    HorizontalAlignment :: Key ()
    JustifyAlignment :: Key ()
    VerticalAlignment :: Key ()
    Upright :: Key ()
    Direction :: Key ()
    AvoidEdges :: Key Bool
    FfSettings :: Key ()

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
  showTaggedPrec TextPlacements_ = showsPrec
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
  eqTagged TextPlacements_ TextPlacements_ = (==)
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



data Symbolizer
  = Point          { _symbolizerProperties :: Properties }
  | Line           { _symbolizerProperties :: Properties }
  | LinePattern    { _symbolizerProperties :: Properties }
  | Polygon        { _symbolizerProperties :: Properties }
  | PolygonPattern { _symbolizerProperties :: Properties }
  | Raster         { _symbolizerProperties :: Properties }
  | Shield         { _symbolizerProperties :: Properties }
  | Text           { _symbolizerProperties :: Properties }
  | Building       { _symbolizerProperties :: Properties }
  | Marker         { _symbolizerProperties :: Properties }
  | Group          { _symbolizerProperties :: Properties }
  | Debug          { _symbolizerProperties :: Properties }
  | Dot            { _symbolizerProperties :: Properties }
  deriving (Eq, Show, Generic)
makeClassy ''Symbolizer
makeFields ''Symbolizer
makePrisms ''Symbolizer

symType :: Symbolizer -> Text
symType Point{} = "point"
symType Line{} = "line"
symType LinePattern{} = "linePattern"
symType Polygon{} = "polygon"
symType PolygonPattern{} = "polygonPattern"
symType Raster{} = "raster"
symType Shield{} = "shield"
symType Text{} = "text"
symType Building{} = "building"
symType Marker{} = "marker"
symType Group{} = "group"
symType Debug{} = "debug"
symType Dot{} = "dot"

toProperties :: Symbolizer -> [DSum Key PropValue]
toProperties = DMap.toList . _symbolizerProperties

instance ToJSON Symbolizer where
  toJSON sym = object (("type",toJSON (symType sym)):map go  (toProperties sym))
    where
      go :: Property -> (Text,Value)
      go (Gamma :=> v) = ("gamma", toJSON v)
      go (GammaMethod :=> v) = ("gammaMethod", toJSON v)
      go (Opacity :=> v) = ("opacity", toJSON v)
      go (Alignment :=> v) = ("alignment", toJSON v)
      go (Offset :=> v) = ("offset", toJSON v)
      go (CompOp :=> v) = ("compOp", toJSON v)
      go (Clip :=> v) = ("clip", toJSON v)
      go (Fill :=> v) = ("fill", toJSON v)
      go (FillOpacity :=> v) = ("fillOpacity", toJSON v)
      go (Stroke :=> v) = ("stroke", toJSON v)
      go (StrokeWidth :=> v) = ("strokeWidth", toJSON v)
      go (StrokeOpacity :=> v) = ("strokeOpacity", toJSON v)
      go (StrokeLinejoin :=> v) = ("strokeLinejoin", toJSON v)
      go (StrokeLinecap :=> v) = ("strokeLinecap", toJSON v)
      go (StrokeGamma :=> v) = ("strokeGamma", toJSON v)
      go (StrokeGammaMethod :=> v) = ("strokeGammaMethod", toJSON v)
      go (StrokeDashoffset :=> v) = ("strokeDashoffset", toJSON v)
      go (StrokeDasharray :=> v) = ("strokeDasharray", toJSON v)
      go (StrokeMiterlimit :=> v) = ("strokeMiterlimit", toJSON v)
      go (GeometryTransform :=> v) = ("geometryTransform", toJSON v)
      go (LineRasterizer :=> v) = ("lineRasterizer", toJSON v)
      go (ImageTransform :=> v) = ("imageTransform", toJSON v)
      go (Spacing :=> v) = ("spacing", toJSON v)
      go (MaxError :=> v) = ("maxError", toJSON v)
      go (AllowOverlap :=> v) = ("allowOverlap", toJSON v)
      go (IgnorePlacement :=> v) = ("ignorePlacement", toJSON v)
      go (Width :=> v) = ("width", toJSON v)
      go (Height :=> v) = ("height", toJSON v)
      go (File :=> v) = ("file", toJSON v)
      go (ShieldDx :=> v) = ("shieldDx", toJSON v)
      go (ShieldDy :=> v) = ("shieldDy", toJSON v)
      go (UnlockImage :=> v) = ("unlockImage", toJSON v)
      go (Mode :=> v) = ("mode", toJSON v)
      go (Scaling :=> v) = ("scaling", toJSON v)
      go (FilterFactor :=> v) = ("filterFactor", toJSON v)
      go (MeshSize :=> v) = ("meshSize", toJSON v)
      go (Premultiplied :=> v) = ("premultiplied", toJSON v)
      go (Smooth :=> v) = ("smooth", toJSON v)
      go (SimplifyAlgorithm :=> v) = ("simplifyAlgorithm", toJSON v)
      go (SimplifyTolerance :=> v) = ("simplifyTolerance", toJSON v)
      go (HaloRasterizer :=> v) = ("haloRasterizer", toJSON v)
      go (TextPlacements_ :=> v) = ("textPlacements_", toJSON v)
      go (LabelPlacement :=> v) = ("labelPlacement", toJSON v)
      go (MarkersPlacementType :=> v) = ("markersPlacementType", toJSON v)
      go (MarkersMultipolicy :=> v) = ("markersMultipolicy", toJSON v)
      go (PointPlacementType :=> v) = ("pointPlacementType", toJSON v)
      go (Colorizer :=> v) = ("colorizer", toJSON v)
      go (HaloTransform :=> v) = ("haloTransform", toJSON v)
      go (NumColumns :=> v) = ("numColumns", toJSON v)
      go (StartColumn :=> v) = ("startColumn", toJSON v)
      go (RepeatKey :=> v) = ("repeatKey", toJSON v)
      go (GroupProperties :=> v) = ("groupProperties", toJSON v)
      go (LargestBoxOnly :=> v) = ("largestBoxOnly", toJSON v)
      go (MinimumPathLength :=> v) = ("minimumPathLength", toJSON v)
      go (HaloCompOp :=> v) = ("haloCompOp", toJSON v)
      go (TextTransform :=> v) = ("textTransform", toJSON v)
      go (HorizontalAlignment :=> v) = ("horizontalAlignment", toJSON v)
      go (JustifyAlignment :=> v) = ("justifyAlignment", toJSON v)
      go (VerticalAlignment :=> v) = ("verticalAlignment", toJSON v)
      go (Upright :=> v) = ("upright", toJSON v)
      go (Direction :=> v) = ("direction", toJSON v)
      go (AvoidEdges :=> v) = ("avoidEdges", toJSON v)
      go (FfSettings :=> v) = ("ffSettings", toJSON v)

instance FromJSON Symbolizer where
  parseJSON = withObject "Symbolizer" $ \o -> do
    type_ :: Text <- o .: "type" 
    props <- DMap.fromList <$> mapM (uncurry parseProperty) (HM.toList (HM.delete "type" o))
    case type_ of
      "point" -> return (Point props)
      "line" -> return (Line props)
      "linePattern" -> return (LinePattern props)
      "polygon" -> return (Polygon props)
      "polygonPattern" -> return (PolygonPattern props)
      "raster" -> return (Raster props)
      "shield" -> return (Shield props)
      "text" -> return (Text props)
      "building" -> return (Building props)
      "marker" -> return (Marker props)
      "group" -> return (Group props)
      "debug" -> return (Debug props)
      "dot" -> return (Dot props)
      _     -> fail "Unknown symbolizer type"

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
parseProperty "textPlacements_" v = (TextPlacements_ :=>) <$> parseJSON v
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
