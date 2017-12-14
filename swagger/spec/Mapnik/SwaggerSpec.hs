{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Mapnik.SwaggerSpec (main, spec) where

import Mapnik
import Mapnik.Swagger ()
import Mapnik.QuickCheck ()

import Data.Aeson (ToJSON, encode)
import Data.Proxy
import Data.Swagger (ToSchema, toSchema)
import Data.Swagger.Schema.Validation (validateToJSON)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "ToJSON matches ToSchema" $ do
  describe "Style" $ swaggerProp(Proxy @Style)
  describe "FilterMode" $ swaggerProp(Proxy @FilterMode)
  describe "Rule" $ swaggerProp(Proxy @Rule)
  describe "Color" $ swaggerProp(Proxy @Color)
  describe "CompositeMode" $ swaggerProp(Proxy @CompositeMode)
  describe "ColorStop" $ swaggerProp(Proxy @ColorStop)
  describe "Symbolizer" $ swaggerProp(Proxy @Symbolizer)
  describe "Expression" $ swaggerProp(Proxy @Expression)
  describe "PathExpression" $ swaggerProp(Proxy @PathExpression)
  describe "PointPlacement" $ swaggerProp(Proxy @PointPlacement)
  describe "Transform" $ swaggerProp(Proxy @Transform)
  describe "SimplifyAlgorithm" $ swaggerProp(Proxy @SimplifyAlgorithm)
  describe "LineRasterizer" $ swaggerProp(Proxy @LineRasterizer)
  describe "GammaMethod" $ swaggerProp(Proxy @GammaMethod)
  describe "Dash" $ swaggerProp(Proxy @Dash)
  describe "LineJoin" $ swaggerProp(Proxy @LineJoin)
  describe "PatternAlignment" $ swaggerProp(Proxy @PatternAlignment)
  describe "ScalingMethod" $ swaggerProp(Proxy @ScalingMethod)
  describe "Colorizer" $ swaggerProp(Proxy @Colorizer)
  describe "ColorizerMode" $ swaggerProp(Proxy @ColorizerMode)
  describe "Stop" $ swaggerProp(Proxy @Stop)
  describe "TextSymProperties" $ swaggerProp(Proxy @TextSymProperties)
  describe "HaloRasterizer" $ swaggerProp(Proxy @HaloRasterizer)
  describe "MarkerPlacement" $ swaggerProp(Proxy @MarkerPlacement)
  describe "TextProperties" $ swaggerProp(Proxy @TextProperties)
  describe "LabelPlacement" $ swaggerProp(Proxy @LabelPlacement)
  describe "TextLayoutProperties" $ swaggerProp(Proxy @TextLayoutProperties)
  describe "MarkerMultiPolicy" $ swaggerProp(Proxy @MarkerMultiPolicy)
  describe "HorizontalAlignment" $ swaggerProp(Proxy @HorizontalAlignment)
  describe "Upright" $ swaggerProp(Proxy @Upright)
  describe "TextFormatProperties" $ swaggerProp(Proxy @TextFormatProperties)
  describe "Font" $ swaggerProp(Proxy @Font)
  describe "FontFeatureSettings" $ swaggerProp(Proxy @FontFeatureSettings)
  describe "PlacementDirection" $ swaggerProp(Proxy @PlacementDirection)
  describe "Direction" $ swaggerProp(Proxy @Direction)
  describe "GroupSymProperties" $ swaggerProp(Proxy @GroupSymProperties)
  describe "GroupLayout" $ swaggerProp(Proxy @GroupLayout)
  describe "DebugMode" $ swaggerProp(Proxy @DebugMode)
  describe "GroupRule" $ swaggerProp(Proxy @GroupRule)
  describe "JustifyAlignment" $ swaggerProp(Proxy @JustifyAlignment)
  describe "VerticalAlignment" $ swaggerProp(Proxy @VerticalAlignment)
  describe "TextTransform" $ swaggerProp(Proxy @TextTransform)
  describe "TextPlacements" $ swaggerProp(Proxy @TextPlacements)
  describe "LineCap" $ swaggerProp(Proxy @LineCap)
  describe "ImageFilter" $ swaggerProp(Proxy @ImageFilter)
  describe "Layer" $ swaggerProp(Proxy @Layer)
  describe "Map" $ swaggerProp(Proxy @Map)
  describe "Datasource" $ swaggerProp(Proxy @Datasource)
  describe "ImageRgba8" $ swaggerProp(Proxy @ImageRgba8)
  describe "PixelRgba8" $ swaggerProp(Proxy @ImageRgba8)

swaggerProp
  :: forall p a.
   ( Show a
   , Eq a
   , ToJSON a
   , ToSchema a
   , Arbitrary a
   )
  => p a
  -> SpecWith ()
swaggerProp _ = parallel $
  prop "json correspondes to schema" $ \(a :: a) ->
    let errors = validateToJSON a
        s = toSchema (Proxy @a)
    in counterexample (show (errors, encode a, encode s, s)) $ errors == []
