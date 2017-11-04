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
{-# LANGUAGE TypeOperators #-}
module Mapnik.Symbolizer where

import Mapnik.Imports
import Mapnik.Expression
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Typeable
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Data.String (fromString)

data PropValue v = PropValue      v
                 | PropExpression Expression
  deriving (Eq, Show)

instance ToJSON v => ToJSON (PropValue v) where
  toJSON (PropValue v)      = toJSON v
  toJSON (PropExpression v) = toJSON v

instance FromJSON v => FromJSON (PropValue v) where
  parseJSON o = (PropValue <$> parseJSON o) <|> (PropExpression <$> parseJSON o)

data Property where
  (:=>) :: (ToJSON v, Show v, Eq v, Typeable v) => Key v -> PropValue v -> Property

(==>) :: (ToJSON v, Show v, Eq v, Typeable v) => Key v -> v -> Property
k ==> v = k :=> PropValue v

(=~>) :: (ToJSON v, Show v, Eq v, Typeable v) => Key v -> Expression -> Property
k =~> v = k :=> PropExpression v

deriving instance Show Property

instance Eq Property where
  (ka :: Key a) :=> a == (kb :: Key b) :=> b = case (eqT :: Maybe (a :~: b)) of
    Just Refl -> ka == kb && a == b
    Nothing   -> False

type Properties = [Property]

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
deriving instance Show (Key a)
deriving instance Eq (Key a)


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

instance ToJSON Symbolizer where
  toJSON sym = object (("type",toJSON (symType sym)):map go  (_symbolizerProperties sym))
    where
      go :: Property -> (Text,Value)
      go (k :=> v) = (fromString (uncapitalize (show k)), toJSON v)

instance FromJSON Symbolizer where
  parseJSON = withObject "Symbolizer" $ \o -> do
    type_ :: Text <- o .: "type" 
    props <- mapM (uncurry parseProperty) (HM.toList (HM.delete "type" o))
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
