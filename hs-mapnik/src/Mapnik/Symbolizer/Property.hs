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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Mapnik.Symbolizer.Property (
  module Mapnik.Symbolizer.Property
, def
) where

import Mapnik.Imports
import Mapnik.Enums
import Mapnik.Common

import Data.Default (Default(def))

--TODO
type FontFeatureSettings = ()
type SimplifyAlgorithm = ()
type GroupProperties = ()
type Scaling = ()
type TextPlacements = ()
type Colorizer = ()

data Prop a = Exp Expression
            | Val a
  deriving (Eq, Show, Functor)
deriveMapnikJSON ''Prop

type PropValue a = Maybe (Prop a)

pattern PropDefault :: Maybe a
pattern PropDefault = Nothing

pattern PropExpression :: Expression -> Maybe (Prop a)
pattern PropExpression e = Just (Exp e)

pattern PropValue :: a -> Maybe (Prop a)
pattern PropValue e = Just (Val e)

data Property where
  (:=>) :: Key v -> Prop v -> Property

type Properties = [Property]

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
    Mode :: Key (Either DebugMode RasterMode)
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
