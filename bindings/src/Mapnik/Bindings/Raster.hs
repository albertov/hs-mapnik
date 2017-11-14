{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Mapnik.Bindings.Raster (Raster(..)) where

import           Mapnik (Box)
import           Mapnik.Bindings
import           Mapnik.Bindings.Orphans()
import           Mapnik.Bindings.Variant

import           Control.Exception (bracket, throwIO)
import qualified Data.Vector.Storable as V
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import           Foreign.Ptr
import           Foreign.Marshal.Utils (with)
import           Data.Int
import           Data.Word

C.context mapnikCtx

C.include "<mapnik/raster.hpp>"
C.include "<mapnik/image_any.hpp>"
C.include "<mapnik/pixel_types.hpp>"
C.include "<mapnik/box2d.hpp>"
C.include "<memory>"

C.using "namespace mapnik"
C.using "raster_ptr = std::shared_ptr<raster>"
C.using "bbox = box2d<double>"
C.using "pixel_rgba8 = rgba8_t::type"
C.using "pixel_gray8 = gray8_t::type"
C.using "pixel_gray8s = gray8s_t::type"
C.using "pixel_gray16 = gray16_t::type"
C.using "pixel_gray16s = gray16s_t::type"
C.using "pixel_gray32 = gray32_t::type"
C.using "pixel_gray32s = gray32s_t::type"
C.using "pixel_gray32f = gray32f_t::type"
C.using "pixel_gray64 = gray64_t::type"
C.using "pixel_gray64s = gray64s_t::type"
C.using "pixel_gray64f = gray64f_t::type"


data Raster a = Raster
  { extent :: !Box
  , queryExtent :: !Box
  , filterFactor :: !Double
  , width  :: !Int
  , height :: !Int
  , nodata :: !(Maybe a)
  , pixels :: !(V.Vector a)
  } deriving (Eq, Show)

withMaybe :: V.Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe Nothing  f = f nullPtr
withMaybe (Just a) f = with a f

instance VariantPtr RasterPtr where
  allocaV = bracket alloc dealloc where
    alloc = [CU.exp|raster_ptr * { new raster_ptr }|]
    dealloc p = [CU.exp|void { delete $(raster_ptr *p)}|]

#define RASTER_VARIANT(HS,CPP,DTYPE) \
instance Variant RasterPtr (Raster HS) where { \
  pokeV _ Raster{ pixels=px, width=w, height=h } \
    | V.length px /= w*h = throwIO (userError "Invalid vector size"); \
  pokeV p Raster{ filterFactor = (realToFrac -> ff) \
                , width        = (fromIntegral -> w) \
                , height       = (fromIntegral -> h) \
                , .. \
                } = with extent $ \ext -> \
                    with queryExtent $ \qext -> \
                    withMaybe nodata $ \nd -> \
    [CU.block|void { \
      image_any image($(int w), $(int h), DTYPE); \
      memcpy(image.bytes(), $vec-ptr:(CPP *pixels), sizeof(CPP)*$vec-len:pixels); \
      auto ptr = *$(raster_ptr *p) = std::make_shared<raster>(*$(bbox *ext), *$(bbox *qext), std::move(image), $(double ff)); \
      if ( $(CPP *nd) ) { \
        ptr->set_nodata(*$(CPP *nd)); \
      } \
    }|]; \
  peekV _TODO = error "TODO" \
}

RASTER_VARIANT(RGBA8,pixel_rgba8,image_dtype_rgba8)
RASTER_VARIANT(Word8,pixel_gray8,image_dtype_gray8)
RASTER_VARIANT(Int8,pixel_gray8s,image_dtype_gray8s)
RASTER_VARIANT(Word16,pixel_gray16,image_dtype_gray16)
RASTER_VARIANT(Int16,pixel_gray16s,image_dtype_gray16s)
RASTER_VARIANT(Word32,pixel_gray32,image_dtype_gray32)
RASTER_VARIANT(Int32,pixel_gray32s,image_dtype_gray32s)
RASTER_VARIANT(Float,pixel_gray32f,image_dtype_gray32f)
RASTER_VARIANT(Word64,pixel_gray64,image_dtype_gray64)
RASTER_VARIANT(Int64,pixel_gray64s,image_dtype_gray64s)
RASTER_VARIANT(Double,pixel_gray64f,image_dtype_gray64f)
