{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
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
, SymbolizerBase (..)
, Expression (..)
, Transform (..)
, TextPlacements (..)
, FontSet(..)
, Color(..)
, Box (..)
, Datasource (..)
, Parameters (..)
, Projection (..)
, ProjTransform (..)
, C.CppException (..)
, MapnikInt
, mapnikCtx
) where

import           Mapnik (Box(..), Color(..))

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import           Data.Monoid (mempty, (<>))
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import           Foreign.ForeignPtr (ForeignPtr)

#define fptr(X) newtype X = X (ForeignPtr X) deriving (Eq, Ord, Show)

fptr(Map)
fptr(Image)
fptr(Layer)
fptr(Datasource)
fptr(Parameters)
fptr(Projection)
fptr(ProjTransform)
fptr(Style)
fptr(Rule)
fptr(Symbolizer)
fptr(SymbolizerBase)
fptr(Expression)
fptr(Transform)
fptr(TextPlacements)
fptr(Colorizer)
fptr(Stop)
fptr(FontFeatureSettings)
fptr(FontSet)
fptr(GroupProperties)

#ifdef BIGINT
type MapnikInt = C.CLong
#else
type MapnikInt = C.CInt
#endif

mapnikCtx :: Context
mapnikCtx = C.baseCtx <> C.cppCtx <> C.bsCtx <> C.fptrCtx <> C.funCtx <> C.vecCtx <> ctx
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
      , (C.TypeName "symbolizer_base", [t| SymbolizerBase |])
      , (C.TypeName "expression_ptr", [t| Expression |])
      , (C.TypeName "transform_type", [t| Transform |])
      , (C.TypeName "keys", [t| C.CUChar |])
      , (C.TypeName "param_type", [t| C.CInt |])
      , (C.TypeName "value_integer", [t| MapnikInt |])
      , (C.TypeName "color", [t| Mapnik.Color |])
      , (C.TypeName "text_placements_ptr", [t| TextPlacements |])
      ]
    }
