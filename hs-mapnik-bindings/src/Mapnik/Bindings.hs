{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
module Mapnik.Bindings (
  Map (..)
, Image (..)
, Layer (..)
, Style (..)
, Rule (..)
, Symbolizer (..)
, Expression (..)
, Box (..)
, Datasource (..)
, Parameters (..)
, Projection (..)
, ProjTransform (..)
, Color (..)
, C.CppException (..)
, mapnikCtx
) where

import           Mapnik (Box(..))

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import           Data.Monoid (mempty, (<>))
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import           Foreign.ForeignPtr (ForeignPtr)


newtype Map = Map (ForeignPtr Map)
newtype Image = Image (ForeignPtr Image)
newtype Layer = Layer (ForeignPtr Layer)
newtype Datasource = Datasource (ForeignPtr Datasource)
newtype Parameters = Parameters (ForeignPtr Parameters)
newtype Projection = Projection (ForeignPtr Projection)
newtype ProjTransform = ProjTransform (ForeignPtr ProjTransform)
newtype Color = Color (ForeignPtr Color)
newtype Style = Style (ForeignPtr Style)
newtype Rule = Rule (ForeignPtr Rule)
newtype Symbolizer = Symbolizer (ForeignPtr Symbolizer)
newtype Expression = Expression (ForeignPtr Expression)

mapnikCtx :: Context
mapnikCtx = C.baseCtx <> C.cppCtx <> C.bsCtx <> C.fptrCtx <> ctx
  where ctx = mempty {
    ctxTypesTable =
      [ (C.TypeName "Map", [t| Map |])
      , (C.TypeName "image_rgba8", [t| Image |])
      , (C.TypeName "layer", [t| Layer |])
      , (C.TypeName "datasource_ptr", [t| Datasource |])
      , (C.TypeName "parameters", [t| Parameters |])
      , (C.TypeName "projection", [t| Projection |])
      , (C.TypeName "proj_transform", [t| ProjTransform |])
      , (C.TypeName "color", [t| Color |])
      , (C.TypeName "feature_type_style", [t| Style |])
      , (C.TypeName "rule", [t| Rule |])
      , (C.TypeName "symbolizer", [t| Symbolizer |])
      , (C.TypeName "expression_ptr", [t| Expression |])
      ]
    }
