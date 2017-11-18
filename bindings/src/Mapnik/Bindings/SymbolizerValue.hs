{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.SymbolizerValue (
  unsafeNew
, unsafeNewMaybe
) where

import qualified Mapnik
import           Mapnik.Enums
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Variant
import qualified Mapnik.Bindings.GroupProperties as GroupProperties
import qualified Mapnik.Bindings.Colorizer as Colorizer
import qualified Mapnik.Bindings.Expression as Expression
import qualified Mapnik.Bindings.Transform as Transform
import           Mapnik.Bindings.Orphans ()

import           Control.Exception
import           Data.Char
import           Data.Text (Text, pack, unpack)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Storable         as V
import           Foreign.Ptr (Ptr, castPtr, nullPtr)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Marshal.Alloc (finalizerFree)
import           Foreign.Marshal.Utils (with)
import           Foreign.Storable
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Language.C.Inline.Unsafe as CU


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/symbolizer_base.hpp>"
C.include "<mapnik/symbolizer.hpp>"
C.include "<mapnik/expression_string.hpp>"
C.include "<mapnik/transform_processor.hpp>"
C.include "<mapnik/util/variant.hpp>"

C.using "namespace mapnik"
C.verbatim "typedef symbolizer_base::value_type sym_value_type;"
C.verbatim "typedef std::pair<double,double> dash_t;"

foreign import ccall "&hs_mapnik_destroy_SymbolizerValue" destroySymbolizerValue :: FinalizerPtr SymbolizerValue

unsafeNew :: (Ptr (Ptr SymbolizerValue) -> IO ()) -> IO SymbolizerValue
unsafeNew = mkUnsafeNew SymbolizerValue destroySymbolizerValue

unsafeNewMaybe :: (Ptr (Ptr SymbolizerValue) -> IO ()) -> IO (Maybe SymbolizerValue)
unsafeNewMaybe = mkUnsafeNewMaybe SymbolizerValue destroySymbolizerValue

instance VariantPtr SymbolizerValue where
  allocaV = bracket alloc dealloc where
    alloc = [CU.exp|sym_value_type * { new sym_value_type }|]
    dealloc p = [CU.exp|void { delete $(sym_value_type *p)}|]


#define SYM_VAL_PTR(HS,CPP) \
instance Variant SymbolizerValue Mapnik.HS where {\
  peekV p = HS.unCreate =<<\
    HS.unsafeNew (\ret ->\
      [CU.block|void {\
      try {\
        *$(CPP **ret) =\
          new CPP(util::get<CPP>(*$(sym_value_type *p)));\
      } catch (std::exception) {\
        *$(CPP **ret) = nullptr;\
      }\
      }|]) ;\
  pokeV p v' = do {\
    v <-HS.create v';\
    [CU.block|void {\
      *$(sym_value_type *p) = sym_value_type(*$fptr-ptr:(CPP *v));\
    }|];}\
};

SYM_VAL_PTR(Colorizer,raster_colorizer_ptr)
SYM_VAL_PTR(GroupProperties,group_symbolizer_properties_ptr)


#define SYM_VAL(HS,CPP,CONV) \
instance Variant SymbolizerValue HS where {\
  pokeV p (CONV -> v) = \
    [CU.block|void { *$(sym_value_type *p) = sym_value_type($(CPP v)); }|]; \
  peekV p = fmap CONV $ justOrTypeError $ newMaybe (\(has,ret) -> \
    [CU.block|void { \
    try { \
      *$(CPP *ret) = util::get<CPP>(*$(sym_value_type *p)); \
      *$(int *has) = 1; \
    } catch (std::exception) { \
      *$(int *has) = 0; \
    } \
    }|]) \
}

#define SYM_VAL_ENUM(HS,CPP) \
instance Variant SymbolizerValue HS where {\
  pokeV p (fromIntegral . fromEnum -> v) = \
    [CU.block|void { *$(sym_value_type *p) = enumeration_wrapper(static_cast<CPP>($(int v)));}|]; \
  peekV p = fmap (toEnum . fromIntegral) $ justOrTypeError $ newMaybe $ \(has,ret) -> \
    [CU.block|void { \
    try { \
      *$(int *ret) = static_cast<int>(util::get<enumeration_wrapper>(*$(sym_value_type *p))); \
      *$(int *has) = 1; \
    } catch (std::exception) { \
      *$(int *has) = 0; \
    } \
    }|] \
}


SYM_VAL(Int,value_integer,fromIntegral)
SYM_VAL(Double,double,realToFrac)
SYM_VAL_ENUM(Bool,bool)
SYM_VAL_ENUM(CompositeMode,composite_mode_e)
SYM_VAL_ENUM(LineCap,line_cap_enum)
SYM_VAL_ENUM(LineJoin,line_join_enum)
SYM_VAL_ENUM(LineRasterizer,line_rasterizer_enum)
SYM_VAL_ENUM(HaloRasterizer,halo_rasterizer_enum)
SYM_VAL_ENUM(PointPlacement,point_placement_enum)
SYM_VAL_ENUM(PatternAlignment,pattern_alignment_enum)
SYM_VAL_ENUM(DebugMode,debug_symbolizer_mode_enum)
SYM_VAL_ENUM(MarkerPlacement,marker_placement_enum)
SYM_VAL_ENUM(MarkerMultiPolicy,marker_multi_policy_enum)
SYM_VAL_ENUM(TextTransform,text_transform_enum)
SYM_VAL_ENUM(LabelPlacement,label_placement_enum)
SYM_VAL_ENUM(VerticalAlignment,vertical_alignment_enum)
SYM_VAL_ENUM(HorizontalAlignment,horizontal_alignment_enum)
SYM_VAL_ENUM(JustifyAlignment,justify_alignment_enum)
SYM_VAL_ENUM(Upright,text_upright_enum)
SYM_VAL_ENUM(Direction,direction_enum)
SYM_VAL_ENUM(GammaMethod,gamma_method_enum)
SYM_VAL_ENUM(ScalingMethod,scaling_method_e)
SYM_VAL_ENUM(SimplifyAlgorithm,simplify_algorithm_e)

instance Variant SymbolizerValue a => Variant SymbolizerValue (Mapnik.Prop a) where
  peekV p = do
    val <- try (peekV p)
    case val of
      Right v -> return (Mapnik.Exp v)
      Left VariantTypeError -> Mapnik.Val <$> peekV p
  pokeV p (Mapnik.Exp a) = pokeV p a
  pokeV p (Mapnik.Val a) = pokeV p a

instance Variant SymbolizerValue Text where
  peekV p = justOrTypeError $
    newTextMaybe $ \(ptr, len) ->
      [CU.block|void {
      try {
        auto v = util::get<std::string>(*$(sym_value_type *p));
        *$(char** ptr) = strdup(v.c_str());
        *$(int* len) = v.length();
      } catch(std::exception) {
        *$(char** ptr) = nullptr;
      }
      }|]
  pokeV p (encodeUtf8 -> v) =
    [CU.block|void {
      *$(sym_value_type *p) = sym_value_type(std::string($bs-ptr:v, $bs-len:v));
    }|]

instance Variant SymbolizerValue Char where
  peekV p = do
    ptr <- C.withPtr_ $ \ret ->
      [CU.block|void {
      try {
        *$(char **ret) = const_cast<char*>(
          util::get<std::string>(*$(sym_value_type *p)).c_str()
          );
      } catch(std::exception) {
        *$(char **ret) = nullptr;
      }
      }|]
    if ptr == nullPtr
      then throwIO VariantTypeError
      else chr . fromIntegral <$> peek ptr

  pokeV p (fromIntegral . ord -> c) =
    [CU.block|void {
      *$(sym_value_type *p) = sym_value_type(std::string(&$(char c), 1));
    }|]

instance Variant SymbolizerValue String where
  peekV = fmap unpack . peekV
  pokeV p = pokeV p . pack

instance Variant SymbolizerValue Mapnik.Expression where
  peekV p = 
    fmap Mapnik.Expression $ justOrTypeError $ newTextMaybe $ \(ret, len) ->
      [CU.block|void {
      try {
        auto expr = util::get<expression_ptr>(*$(sym_value_type *p));
        if (expr) {
          std::string s = to_expression_string(*expr);
          *$(char** ret) = strdup(s.c_str());
          *$(int* len) = s.length();
        } else {
          *$(char** ret) = nullptr;
        }
      } catch (std::exception) {
        *$(char** ret) = nullptr;
      }
      }|]
  pokeV p (Mapnik.Expression expr) =
    case Expression.parse expr of
      Right v ->
        [CU.block|void{ *$(sym_value_type *p) = sym_value_type(*$fptr-ptr:(expression_ptr *v)); }|]
      Left e -> throwIO (userError e)

instance Variant SymbolizerValue Mapnik.FontFeatureSettings where
  peekV p =
    fmap Mapnik.FontFeatureSettings $ justOrTypeError $ newTextMaybe $ \(ptr, len) ->
      [CU.block|void {
      try {
        auto v = util::get<font_feature_settings>(*$(sym_value_type *p));
        std::string s = v.to_string();
        *$(char** ptr) = strdup(s.c_str());
        *$(int* len) = s.length();
      } catch(std::exception) {
        *$(char** ptr) = nullptr;
      }
      }|]
  pokeV p (Mapnik.FontFeatureSettings (encodeUtf8 -> v)) =
    [C.catchBlock|
      *$(sym_value_type *p) = sym_value_type(font_feature_settings(std::string($bs-ptr:v, $bs-len:v)));
    |]

instance Variant SymbolizerValue Mapnik.DashArray where
  pokeV p dashes =
    [CU.block|void {
      std::vector<dash_t> dashes($vec-len:dashes);
      for (int i=0; i<$vec-len:dashes; i++) {
        dashes[i] = $vec-ptr:(dash_t *dashes)[i];
      }
      *$(sym_value_type *p) = sym_value_type(dashes);
    }|]

  peekV p = do
    (fromIntegral -> len, castPtr -> ptr) <- C.withPtrs_ $ \(len,ptr) ->
      [CU.block|void{
      try {
        auto arr = util::get<dash_array>(*$(sym_value_type *p));
        *$(size_t *len) = arr.size();
        dash_t *dashes = *$(dash_t **ptr) =
          static_cast<dash_t *>(malloc(arr.size()*sizeof(dash_t)));
        int i=0;
        for (dash_array::const_iterator it=arr.begin(); it!=arr.end(); ++it, ++i) {
          dashes[i] = *it;
        }
      } catch(std::exception) {
        *$(dash_t **ptr) = nullptr;
      }
      }|]
    if ptr /= nullPtr then do
      fp <- newForeignPtr finalizerFree ptr
      V.freeze (VM.unsafeFromForeignPtr0 fp len)
    else throwIO VariantTypeError

instance Variant SymbolizerValue Mapnik.Color where
  peekV p = justOrTypeError $ newMaybe $ \(has,ret) ->
    [CU.block|void {
    try {
      *$(color *ret) = util::get<color>(*$(sym_value_type *p));
      *$(int *has) = 1;
    } catch(std::exception) {
      *$(int *has) = 0;
    }
    }|]
  pokeV p c = with c $ \cPtr ->
    [CU.block|void {*$(sym_value_type *p) = *$(color *cPtr);}|]

instance Variant SymbolizerValue Mapnik.Transform where
  pokeV p (Mapnik.Transform expr) =
    case Transform.parse expr of
      Right v ->
        [CU.block|void {*$(sym_value_type *p) = sym_value_type(*$fptr-ptr:(transform_type *v));}|]
      Left e -> throwIO (userError e)

  peekV p =
    fmap Mapnik.Transform $ justOrTypeError $ newTextMaybe $ \(ptr, len) ->
      [CU.block|void {
      try {
        auto t = util::get<transform_type>(*$(sym_value_type *p));
        if (t) {
          std::string s = transform_processor_type::to_string(*t);
          *$(char** ptr) = strdup(s.c_str());
          *$(int* len) = s.length();
        } else {
          *$(char** ptr) = nullptr;
        }
      } catch(std::exception) {
        *$(char** ptr) = nullptr;
      }
      }|]
