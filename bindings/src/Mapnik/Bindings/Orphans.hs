{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Mapnik.Bindings.Orphans (
) where

import qualified Mapnik
import           Mapnik (Value(..))
import           Mapnik.Bindings
import           Mapnik.Bindings.Variant

import           Control.Exception (bracket, throwIO)
import           Data.Text.Foreign (useAsPtr)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.ByteString.Unsafe (unsafePackMallocCString)
import           Foreign.Storable (Storable(..))
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.Marshal.Alloc (alloca)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

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
  sizeOf _ = fromIntegral [CU.pure|size_t { sizeof(color) }|]
  alignment _ = fromIntegral [CU.pure|size_t { alignof (color) }|]
  peek p = do
    (   fromIntegral -> r
      , fromIntegral -> g
      , fromIntegral -> b
      , fromIntegral -> a
      ) <- C.withPtrs_ $ \(r,g,b,a) -> [CU.block|void {
      color const& c = *$(color *p);
      *$(unsigned char *r) = c.red();
      *$(unsigned char *g) = c.green();
      *$(unsigned char *b) = c.blue();
      *$(unsigned char *a) = c.alpha();
      }|]
    return (Mapnik.RGBA r g b a)

  poke p (Mapnik.RGBA (fromIntegral ->r) (fromIntegral -> g) (fromIntegral -> b) (fromIntegral -> a)) =
    [CU.block|void{
      *$(color* p) = color($(unsigned char r), $(unsigned char g), $(unsigned char b), $(unsigned char a));
      }|]

instance Storable Mapnik.Box where
  sizeOf _ = fromIntegral [CU.pure|size_t { sizeof(bbox) }|]
  alignment _ = fromIntegral [CU.pure|size_t { alignof (bbox) }|]
  peek p = do
    (   realToFrac -> minx
      , realToFrac -> miny
      , realToFrac -> maxx
      , realToFrac -> maxy
      ) <- C.withPtrs_ $ \(minx,miny,maxx,maxy) -> [CU.block|void {
      bbox const& c = *$(bbox *p);
      *$(double *minx) = c.minx();
      *$(double *miny) = c.miny();
      *$(double *maxx) = c.maxx();
      *$(double *maxy) = c.maxy();
      }|]
    return Mapnik.Box{..}

  poke p (Mapnik.Box (realToFrac -> minx) (realToFrac -> miny) (realToFrac -> maxx) (realToFrac -> maxy)) =
    [CU.block|void{
      *$(bbox* p) = bbox($(double minx), $(double miny), $(double maxx), $(double maxy));
      }|]

instance VariantPtr Value where
  allocaV = bracket alloc dealloc where
    alloc = [CU.exp|value * { new value }|]
    dealloc p = [CU.exp|void { delete $(value *p)}|]

instance Variant Value Value where
  pokeV p (TextValue t) = useAsPtr t $ \(castPtr -> v) (fromIntegral -> len) ->
    [CU.block|void{
      *$(value *p) = UnicodeString (($(unsigned short *v)), $(int len));
    }|]
  pokeV p (IntValue (fromIntegral -> v)) =
    [CU.block|void{ *$(value *p) = $(value_integer v); }|]
  pokeV p (DoubleValue (realToFrac -> v)) =
    [CU.block|void{ *$(value *p) = $(double v); }|]
  pokeV p (BoolValue (fromIntegral . fromEnum -> v)) =
    [CU.block|void{ *$(value *p) = $(int v) ? true : false ; }|]
  pokeV p NullValue =
    [CU.block|void{ *$(value *p) = value_null(); }|]

  peekV p = alloca $ \res -> alloca $ \ty -> do
    [CU.block|void{
      util::apply_visitor(value_extractor_visitor($(void *res), $(int *ty)), *$(value *p));
    }|]
    extractVal res =<< peek ty

extractVal :: Ptr () -> C.CInt -> IO Value
extractVal res type_ =
    case type_ of
      0 -> return NullValue
      1 -> DoubleValue . realToFrac   <$> peek @C.CDouble (castPtr res)
      2 -> IntValue    . fromIntegral <$> peek @C.CInt    (castPtr res)
      3 -> BoolValue   . toEnum . fromIntegral <$> peek @C.CInt    (castPtr res)
      4 -> TextValue . decodeUtf8
       <$> (unsafePackMallocCString =<< peek (castPtr res))
      _ -> throwIO VariantTypeError

instance VariantPtr Param where
  allocaV = bracket alloc dealloc where
    alloc = [CU.exp|value_holder * { new value_holder }|]
    dealloc p = [CU.exp|void { delete $(value_holder *p)}|]

instance Variant Param Value where
  pokeV p (TextValue (encodeUtf8 -> v)) =
    [CU.block|void{ *$(value_holder *p) = std::string ($bs-ptr:v, $bs-len:v); }|]
  pokeV p (IntValue (fromIntegral -> v)) =
    [CU.block|void{ *$(value_holder *p) = $(value_integer v); }|]
  pokeV p (DoubleValue (realToFrac -> v)) =
    [CU.block|void{ *$(value_holder *p) = $(double v); }|]
  pokeV p (BoolValue (fromIntegral . fromEnum -> v)) =
    [CU.block|void{ *$(value_holder *p) = $(int v) ? true : false ; }|]
  pokeV p NullValue =
    [CU.block|void{ *$(value_holder *p) = value_null(); }|]

  peekV p = alloca $ \res -> alloca $ \ty -> do
    [CU.block|void{
      util::apply_visitor(value_extractor_visitor($(void *res), $(int *ty)), *$(value_holder *p));
    }|]
    extractVal res =<< peek ty
