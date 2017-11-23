{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Mapnik.Bindings.Types (
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
, TextProperties
, TextLayoutProperties
, TextFormatProperties
, Format (..)
, FormatNode
, LayoutNode
, ListNode
, Stop
, TextSymProperties (..)
, GroupSymProperties (..)
, FontSet
, Colorizer(..)
, Box
, Datasource (..)
, Parameters (..)
, ProjTransform (..)
, Dash(..)
, FeatureList
, QueryPtr
, Geometry (..)
, FeaturePtr (..)
, PixelRgba8 (..)
, Pair (..)
, MapnikError (..)
, RasterPtr
, Value
, Param
, Attributes
, FeatureCtx
, MapnikInt
, mapnikCtx
) where

import           Mapnik ( Color, Dash(..), GroupRule, Box, Value
                        , PathExpression)
import           Mapnik.Lens ( HasRed(..), HasBlue(..), HasGreen(..)
                              , HasAlpha(..))

import           Control.Lens (lens)
import           Control.Exception
import           Data.Typeable
import           Data.Bits
import qualified Language.C.Inline as C
import           Data.Monoid (mempty, (<>))
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import           Data.Word
import           Data.Int
import           Data.Text
import qualified Data.HashMap.Strict as M
import           Foreign.Storable (Storable)
import           Foreign.ForeignPtr (ForeignPtr)
import           Foreign.Ptr (Ptr, FunPtr)
import qualified Language.C.Inline.Cpp as C

#define fptr(X) newtype X = X (ForeignPtr X) deriving (Eq, Ord, Show)

fptr(Map)
fptr(Image)
fptr(Layer)
fptr(Datasource)
fptr(Parameters)
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
fptr(FontSet)
fptr(GroupSymProperties)
fptr(TextProperties)
fptr(TextFormatProperties)
fptr(TextLayoutProperties)
fptr(TextSymProperties)
fptr(SymbolizerValue)
fptr(Format)
fptr(FormatNode)
fptr(LayoutNode)
fptr(ListNode)
fptr(QueryPtr)
fptr(FeaturePtr)
fptr(FeatureList)
fptr(FeatureCtx)
fptr(RasterPtr)
fptr(Param)
newtype Geometry = Geometry (ForeignPtr Geometry) deriving (Eq, Ord)


#ifdef BIGINT
type MapnikInt = C.CLong
#else
type MapnikInt = C.CInt
#endif

data MapnikError = ConfigError String
                 | FromMapnikError String
                 | CppStdException String
                 | InternalError String
  deriving (Eq, Ord, Show, Typeable, Exception)

data Pair = Pair { x, y :: !Double }
  deriving (Eq, Show)

newtype PixelRgba8 = PixelRgba8 { unRgba8 :: Word32 }
  deriving newtype (Eq, Show, Storable)

#define COLOR_LENS(cname, name, off) \
instance cname PixelRgba8 Word8 where {\
  {-# INLINE name #-}; \
  name = lens \
    (fromIntegral . (.&. 0xFF) . (`unsafeShiftR` off) . unRgba8) \
    (\(PixelRgba8 c) v -> PixelRgba8 (c .&. complement (0xFF `unsafeShiftL` off) .|. (fromIntegral v `unsafeShiftL` off))) \
  };

COLOR_LENS(HasRed,   red,    0)
COLOR_LENS(HasGreen, green,  8)
COLOR_LENS(HasBlue,  blue,  16)
COLOR_LENS(HasAlpha, alpha, 24)

type Attributes = M.HashMap Text Mapnik.Value

mapnikCtx :: Context
mapnikCtx = C.baseCtx <> C.cppCtx <> C.bsCtx <> C.fptrCtx <> C.funCtx <> C.vecCtx <> ctx
  where ctx = mempty {
    ctxTypesTable =
      [ (C.TypeName "Map", [t| Map |])
      , (C.TypeName "image_rgba8", [t| Image |])
      , (C.TypeName "layer", [t| Layer |])
      , (C.TypeName "datasource_ptr", [t| Datasource |])
      , (C.TypeName "parameters", [t| Parameters |])
      , (C.TypeName "hs_proj_transform", [t| ProjTransform |])
      , (C.TypeName "feature_type_style", [t| Style |])
      , (C.TypeName "rule", [t| Rule |])
      , (C.TypeName "symbolizer", [t| Symbolizer |])
      , (C.TypeName "symbolizer_base", [t| SymbolizerBase |])
      , (C.TypeName "expression_ptr", [t| Expression |])
      , (C.TypeName "path_expression_ptr", [t| Mapnik.PathExpression |])
      , (C.TypeName "transform_type", [t| Transform |])
      , (C.TypeName "keys", [t| C.CUChar |])
      , (C.TypeName "value_type", [t| C.CInt |])
      , (C.TypeName "value_integer", [t| MapnikInt |])
      , (C.TypeName "color", [t| Mapnik.Color |])
      , (C.TypeName "text_placements_ptr", [t| TextPlacements |])
      , (C.TypeName "dash_t", [t| Dash |])
      , (C.TypeName "group_symbolizer_properties_ptr", [t| GroupSymProperties |])
      , (C.TypeName "group_rule_ptr", [t| Mapnik.GroupRule |])
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
      , (C.TypeName "raster_ptr", [t| RasterPtr |])
      , (C.TypeName "feature_list", [t| FeatureList |])
      , (C.TypeName "query", [t| QueryPtr |])
      , (C.TypeName "feature_ptr", [t| FeaturePtr |])
      , (C.TypeName "geometry_t", [t| Geometry |])
      , (C.TypeName "context_ptr", [t| FeatureCtx |])
      , (C.TypeName "value", [t| Value |])
      , (C.TypeName "value_holder", [t| Param |])
      , (C.TypeName "bbox", [t| Box |])
      , (C.TypeName "font_set", [t| FontSet |])
      , (C.TypeName "attributes", [t| Attributes |])
      , (C.TypeName "features_callback", [t|FunPtr (Ptr FeatureCtx -> Ptr FeatureList -> Ptr QueryPtr -> IO (Ptr ()))|])
      , (C.TypeName "features_at_point_callback", [t|FunPtr (Ptr FeatureCtx -> Ptr FeatureList -> C.CDouble -> C.CDouble -> C.CDouble -> IO (Ptr ()))|])
      , (C.TypeName "pixel_rgba8", [t| PixelRgba8 |])
      , (C.TypeName "pixel_gray8", [t| Word8 |])
      , (C.TypeName "pixel_gray8s", [t| Int8 |])
      , (C.TypeName "pixel_gray16", [t| Word16 |])
      , (C.TypeName "pixel_gray16s", [t| Int16 |])
      , (C.TypeName "pixel_gray32", [t| Word32 |])
      , (C.TypeName "pixel_gray32s", [t| Int32 |])
      , (C.TypeName "pixel_gray32f", [t| Float |])
      , (C.TypeName "pixel_gray64", [t| Word64 |])
      , (C.TypeName "pixel_gray64s", [t| Int64 |])
      , (C.TypeName "pixel_gray64f", [t| Double |])
      , (C.TypeName "string", [t| String |])
      , (C.TypeName "opt_string", [t| Maybe String |])
      , (C.TypeName "opt_fontset", [t| Maybe FontSet |])
      ]
    }
