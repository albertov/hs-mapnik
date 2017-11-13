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
, SymbolizerBase
, SymbolizerValue (..)
, Expression (..)
, Transform (..)
, TextPlacements (..)
, TextPlacementsBase
, TextProperties
, TextLayoutProperties
, TextFormatProperties
, Format (..)
, FormatNode
, LayoutNode
, ListNode
, Stop
, TextSymProperties (..)
, GroupProperties (..)
, FontSet(..)
, Color(..)
, Colorizer(..)
, Box (..)
, Datasource (..)
, Parameters (..)
, Projection (..)
, ProjTransform (..)
, C.CppException (..)
, Dash(..)
, FeatureList (..)
, Query
, MapnikInt
, mapnikCtx
) where

import           Mapnik (Box(..), Color(..), Dash(..))

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import           Data.Monoid (mempty, (<>))
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import           Foreign.ForeignPtr (ForeignPtr)
import           Foreign.Ptr (Ptr, FunPtr)

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
fptr(TextPlacementsBase)
fptr(Colorizer)
fptr(Stop)
fptr(FontSet)
fptr(GroupProperties)
fptr(TextProperties)
fptr(TextFormatProperties)
fptr(TextLayoutProperties)
fptr(TextSymProperties)
fptr(SymbolizerValue)
fptr(Format)
fptr(FormatNode)
fptr(LayoutNode)
fptr(ListNode)
fptr(Query)
fptr(FeatureList)

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
      , (C.TypeName "text_placements", [t| TextPlacementsBase |])
      , (C.TypeName "dash_t", [t| Dash |])
      , (C.TypeName "group_symbolizer_properties_ptr", [t| GroupProperties |])
      , (C.TypeName "raster_colorizer_ptr", [t| Colorizer |])
      , (C.TypeName "colorizer_stop", [t| Stop |])
      , (C.TypeName "text_symbolizer_properties", [t| TextSymProperties |])
      , (C.TypeName "text_properties_expressions", [t| TextProperties |])
      , (C.TypeName "text_layout_properties", [t| TextLayoutProperties |])
      , (C.TypeName "format_properties", [t| TextFormatProperties |])
      , (C.TypeName "node_ptr", [t| Format |])
      , (C.TypeName "format_node", [t| FormatNode |])
      , (C.TypeName "layout_node", [t| LayoutNode |])
      , (C.TypeName "list_node", [t| ListNode |])
      , (C.TypeName "sym_value_type", [t| SymbolizerValue |])
      , (C.TypeName "bool", [t| Bool |])
      , (C.TypeName "feature_list", [t| FeatureList |])
      , (C.TypeName "query", [t| Query |])
      , (C.TypeName "features_callback", [t|FunPtr (Ptr FeatureList -> Ptr Query -> IO ())|])
      , (C.TypeName "features_at_point_callback", [t|FunPtr (Ptr FeatureList -> C.CDouble -> C.CDouble -> C.CDouble -> IO ())|])
      ]
    }
