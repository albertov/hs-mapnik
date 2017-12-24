{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Swagger () where

import Mapnik hiding (properties)

import Control.Lens
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import qualified Data.Swagger as Sw
import Data.Swagger
import Data.Swagger.Internal.Schema (GToSchema)
import Data.Swagger.Internal.TypeShape (GenericShape, GenericHasSimpleShape)
import Data.Swagger.Declare (DeclareT)
import GHC.Generics (Generic, Rep)


deriving instance ToSchema Style
deriving instance ToSchema FilterMode
deriving instance ToSchema Rule
deriving instance ToSchema Color
deriving instance ToSchema CompositeMode
deriving instance ToSchema ColorStop
instance ToSchema Symbolizer where declareNamedSchema = allNullableSumSchema
deriving instance ToSchema Expression
deriving instance ToSchema PathExpression
deriving instance ToSchema PointPlacement
deriving instance ToSchema Transform
deriving instance ToSchema SimplifyAlgorithm
deriving instance ToSchema LineRasterizer
deriving instance ToSchema GammaMethod
deriving instance ToSchema Dash
deriving instance ToSchema LineJoin
deriving instance ToSchema PatternAlignment
deriving instance ToSchema ScalingMethod
deriving instance ToSchema Colorizer
deriving instance ToSchema ColorizerMode
deriving instance ToSchema Stop
instance ToSchema TextSymProperties where declareNamedSchema = allNullableSumSchema
deriving instance ToSchema HaloRasterizer
deriving instance ToSchema MarkerPlacement
instance ToSchema TextProperties where declareNamedSchema = allNullableSumSchema
deriving instance ToSchema LabelPlacement
instance ToSchema TextLayoutProperties where declareNamedSchema = allNullableSumSchema
deriving instance ToSchema MarkerMultiPolicy
deriving instance ToSchema HorizontalAlignment
deriving instance ToSchema Upright
instance ToSchema TextFormatProperties where declareNamedSchema = allNullableSumSchema
deriving instance ToSchema Font
deriving instance ToSchema FontFeatureSettings
deriving instance ToSchema PlacementDirection
deriving instance ToSchema Direction
instance ToSchema GroupSymProperties where declareNamedSchema = allNullableSumSchema
instance ToSchema GroupLayout where declareNamedSchema = allNullableSumSchema
deriving instance ToSchema DebugMode
instance ToSchema GroupRule where declareNamedSchema = allNullableSumSchema
deriving instance ToSchema JustifyAlignment
deriving instance ToSchema VerticalAlignment
deriving instance ToSchema TextTransform
instance ToSchema TextPlacements where declareNamedSchema = allNullableSumSchema
deriving instance ToSchema LineCap
deriving instance ToSchema (Layer Datasource)
deriving instance ToSchema (Map Datasource)
deriving instance ToSchema Datasource
deriving instance ToSchema Box
deriving instance ToSchema PixelRgba8
deriving instance ToSchema ImageRgba8

instance ToSchema Value where
  declareNamedSchema =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema ImageFilter where
  declareNamedSchema =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToSchema Mapnik.Format where
  declareNamedSchema =
    genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

instance ToSchema a => ToSchema (Prop a) where
  declareNamedSchema _ = do
    schemaVal <- declareNamedSchema (Proxy @a)
    schemaExp    <- declareNamedSchema (Proxy @Expression)
    let schema_ = mempty
          & properties .~ [("Exp", Inline (schemaExp^.schema))
                          ,("Val", Inline (schemaVal^.schema))]
          & minProperties ?~ 1
          & maxProperties ?~ 1
          & paramSchema.type_ .~ SwaggerObject

        name_ = fmap ("Prop" <>) (schemaVal^. Sw.name)
    return $ NamedSchema name_ schema_

allNullableSumSchema
  :: ( GToSchema (Rep a)
     , Generic a
     , GenericHasSimpleShape
        a
        "genericDeclareNamedSchemaUnrestricted"
        (GenericShape (Rep a))
     ) => proxy a -> DeclareT (Definitions Schema) Identity NamedSchema
allNullableSumSchema p = do
  s <- genericDeclareNamedSchema defaultSchemaOptions p
  return $ s
    & schema.required .~ []
    & schema.properties.traverse._Inline.required .~ []
