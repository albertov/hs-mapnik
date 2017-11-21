{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Mapnik.Bindings.Orphans (
) where

import qualified Mapnik
import           Mapnik.Common (Box(..))
import           Mapnik (Value(..))
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Variant
import qualified Mapnik.Bindings.Cpp as C

import           Control.Monad.Base (liftBase)
import           Control.Exception.Lifted (bracket)
import           Data.Text.Foreign
import           Foreign.Storable (Storable(..))
import           Foreign.Ptr (Ptr, castPtr)


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/color.hpp>"
C.include "<mapnik/box2d.hpp>"
C.include "<mapnik/color_factory.hpp>"
C.include "<mapnik/feature.hpp>"
C.include "<mapnik/unicode.hpp>"
C.include "<mapnik/params.hpp>"
C.include "value_util.hpp"

C.using "namespace mapnik"

C.verbatim "typedef box2d<double> bbox;"


instance Storable Mapnik.Color where
  sizeOf _ = fromIntegral [C.pure|size_t { sizeof(color) }|]
  alignment _ = fromIntegral [C.pure|size_t { alignof (color) }|]
  peek p = do
    (   fromIntegral -> r
      , fromIntegral -> g
      , fromIntegral -> b
      , fromIntegral -> a
      ) <- C.withPtrs_ $ \(r,g,b,a) -> [C.block|void {
      color const& c = *$(color *p);
      *$(unsigned char *r) = c.red();
      *$(unsigned char *g) = c.green();
      *$(unsigned char *b) = c.blue();
      *$(unsigned char *a) = c.alpha();
      }|]
    return (Mapnik.RGBA r g b a)

  poke p (Mapnik.RGBA (fromIntegral ->r) (fromIntegral -> g) (fromIntegral -> b) (fromIntegral -> a)) =
    [C.block|void{
      *$(color* p) = color($(unsigned char r), $(unsigned char g), $(unsigned char b), $(unsigned char a));
      }|]

instance Storable Box where
  sizeOf _ = fromIntegral [C.pure|size_t { sizeof(bbox) }|]
  alignment _ = fromIntegral [C.pure|size_t { alignof (bbox) }|]
  peek p = do
    (   realToFrac -> minx
      , realToFrac -> miny
      , realToFrac -> maxx
      , realToFrac -> maxy
      ) <- C.withPtrs_ $ \(minx,miny,maxx,maxy) -> [C.block|void {
      bbox const& c = *$(bbox *p);
      *$(double *minx) = c.minx();
      *$(double *miny) = c.miny();
      *$(double *maxx) = c.maxx();
      *$(double *maxy) = c.maxy();
      }|]
    return Box{..}

  poke p (Box (realToFrac -> x0) (realToFrac -> y0) (realToFrac -> x1) (realToFrac -> y1)) =
    [C.block|void{
      *$(bbox* p) = bbox($(double x0), $(double y0), $(double x1), $(double y1));
      }|]


instance VariantPtr Value where
  allocaV = bracket alloc dealloc where
    alloc = [C.exp|value * { new value }|]
    dealloc p = [C.exp|void { delete $(value *p)}|]

instance VariantPtr Param where
  allocaV = bracket alloc dealloc where
    alloc = [C.exp|value_holder * { new value_holder }|]
    dealloc p = [C.exp|void { delete $(value_holder *p)}|]

instance Variant Value Value where
  pokeV p (TextValue t) = useAsPtr t $ \(castPtr -> v) (fromIntegral -> len) ->
    [C.exp|void{ *$(value *p) = UnicodeString (($(unsigned short *v)), $(int len)) }|]
  pokeV p (IntValue (fromIntegral -> v)) =
    [C.exp|void{ *$(value *p) = $(value_integer v) }|]
  pokeV p (DoubleValue (realToFrac -> v)) =
    [C.exp|void{ *$(value *p) = $(double v) }|]
  pokeV p (BoolValue (fromIntegral . fromEnum -> v)) =
    [C.exp|void{ *$(value *p) = $(int v) ? true : false }|]
  pokeV p NullValue =
    [C.exp|void{ *$(value *p) = value_null()}|]

  peekV p = peekVwith $ \(res,ty,len) ->
    [C.exp|void{
      util::apply_visitor(value_extractor_visitor(const_cast<void const**>($(void **res)), $(value_type *ty), $(int *len)),
                          *$(value *p))
    }|]

instance Variant Param Value where
  pokeV p (TextValue t) = withCStringLen t $ \(s, fromIntegral -> l) ->
    [C.exp|void{ *$(value_holder *p) = std::string ($(char *s), $(int l)) }|]
  pokeV p (IntValue (fromIntegral -> v)) =
    [C.exp|void{ *$(value_holder *p) = $(value_integer v) }|]
  pokeV p (DoubleValue (realToFrac -> v)) =
    [C.exp|void{ *$(value_holder *p) = $(double v) }|]
  pokeV p (BoolValue (fromIntegral . fromEnum -> v)) =
    [C.exp|void{ *$(value_holder *p) = $(int v) ? true : false }|]
  pokeV p NullValue =
    [C.exp|void{ *$(value_holder *p) = value_null() }|]

  peekV p = liftBase $ peekVwith $ \(res,ty,len) ->
    [C.exp|void{
      util::apply_visitor(value_extractor_visitor(const_cast<void const**>($(void **res)), $(value_type *ty), $(int *len)),
                          *$(value_holder *p))
    }|]

data ValueType
  = NullValueT
  | DoubleValueT
  | IntValueT
  | BoolValueT
  | StringValueT
  | UnicodeStringValueT
  deriving Enum

peekVwith :: ((Ptr (Ptr ()), Ptr C.CInt, Ptr C.CInt) -> IO ()) -> IO Value
peekVwith f = do
  (res,type_,sz) <- C.withPtrs_ f
  case toEnum (fromIntegral type_) of
    NullValueT -> return NullValue
    DoubleValueT -> DoubleValue . realToFrac   <$> peek @C.CDouble (castPtr res)
    IntValueT -> IntValue    . fromIntegral <$> peek @MapnikInt (castPtr res)
    BoolValueT -> BoolValue   . toEnum . fromIntegral <$> peek @C.CUChar (castPtr res)
    StringValueT -> TextValue <$> peekCStringLen (castPtr res, fromIntegral sz)
    UnicodeStringValueT -> TextValue <$> fromPtr (castPtr res) (fromIntegral sz)
