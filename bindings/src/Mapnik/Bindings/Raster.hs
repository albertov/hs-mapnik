{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Mapnik.Bindings.Raster (
  Raster
, mkRaster
, RasterType
, SomeRaster(..)
, getPixels
, HasExtent(..)
, HasQueryExtent(..)
, HasNodata(..)
) where

import           Mapnik (Box, PixelRgba8(..))
import           Mapnik.Lens
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Orphans()
import           Mapnik.Bindings.Variant
import qualified Mapnik.Bindings.Cpp as C

import           Control.Exception.Lifted (bracket, throwIO)
import           Control.Lens
import           Data.Typeable
import qualified Data.Vector.Storable as V
import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Foreign.Storable
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


type RasterType a =
  ( Variant RasterPtr (Raster a)
  , V.Storable a
  , Show a
  , Eq a
  , Typeable a
  )

data SomeRaster where
  SomeRaster :: RasterType a => Raster a -> SomeRaster
deriving instance Show SomeRaster

instance Eq SomeRaster where
  SomeRaster (a :: Raster a) == SomeRaster (b :: Raster b) =
    case eqT :: Maybe (a :~: b) of
      Just Refl -> a == b
      Nothing   -> False

mkRaster :: Box -> (Int,Int) -> V.Vector a -> Raster a
mkRaster b (w,h) = Raster b b 1 w h Nothing

data Raster a = Raster
  { _rasterExtent :: !Box
  , _rasterQueryExtent :: !Box
  , _rasterFilterFactor :: !Double
  , _rasterWidth  :: !Int
  , _rasterHeight :: !Int
  , _rasterNodata :: !(Maybe Double)
  , _rasterPixels :: !(V.Vector a)
  } deriving (Eq, Show)
makeFields ''Raster

getPixels :: forall a. RasterType a => SomeRaster -> Maybe (V.Vector a)
getPixels (SomeRaster (r :: Raster b)) = case eqT :: Maybe (a :~: b) of
  Just Refl -> Just (r^.pixels)
  Nothing -> Nothing

withMaybe :: V.Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe Nothing  f = f nullPtr
withMaybe (Just a) f = with a f

instance VariantPtr RasterPtr where
  allocaV = bracket alloc dealloc where
    alloc = [C.exp|raster_ptr * { new raster_ptr }|]
    dealloc p = [C.exp|void { delete $(raster_ptr *p)}|]

#define RASTER_VARIANT(HS,CPP,DTYPE) \
instance Variant RasterPtr (Raster HS) where { \
  pokeV _ Raster{ _rasterPixels=px, _rasterWidth=w, _rasterHeight=h } \
    | V.length px /= w*h = throwIO (userError "Invalid vector size"); \
  pokeV p Raster{ _rasterFilterFactor = (realToFrac -> ff) \
                , _rasterWidth        = (fromIntegral -> w) \
                , _rasterHeight       = (fromIntegral -> h) \
                , .. \
                } = with _rasterExtent $ \ext -> \
                    with _rasterQueryExtent $ \qext -> \
                    withMaybe (fmap realToFrac _rasterNodata) $ \nd -> \
    [C.block|void { \
      image_any image($(int w), $(int h), DTYPE); \
      memcpy(image.bytes(), $vec-ptr:(CPP *_rasterPixels), sizeof(CPP)*$vec-len:_rasterPixels); \
      auto ptr = *$(raster_ptr *p) = std::make_shared<raster>(*$(bbox *ext), *$(bbox *qext), std::move(image), $(double ff)); \
      if ( $(double *nd) ) { \
        ptr->set_nodata(*$(double *nd)); \
      } \
    }|]; \
  peekV p = do {\
    SomeRaster (r :: Raster b) <- peekV p; \
    case eqT :: Maybe (HS :~: b) of { \
      Just Refl -> return r; \
      Nothing   -> throwIO (VariantTypeError "RasterPtr"); \
      }\
    };\
}


RASTER_VARIANT(PixelRgba8,pixel_rgba8,image_dtype_rgba8)
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

-- Must be in sync with mapnik/pixel_types.hpp!
data DType
  = DTypeRgba8
  | DTypeGray8
  | DTypeGray8s
  | DTypeGray16
  | DTypeGray16s
  | DTypeGray32
  | DTypeGray32s
  | DTypeGray32f
  | DTypeGray64
  | DTypeGray64s
  | DTypeGray64f
  | DTypeNull
  deriving (Eq, Show, Enum)

#define DTYPE_CASE(ENUM,TY) \
  ENUM -> \
    let _rasterPixels = V.unsafeFromForeignPtr0 (castForeignPtr bytes) sz \
    in return $ SomeRaster (Raster {..} :: Raster TY)

instance Variant RasterPtr SomeRaster where
  pokeV p (SomeRaster r) = pokeV p r
  peekV p = do
    numBytes <- fromIntegral <$>
      [C.block|size_t {
      const raster_ptr &raster = *$(raster_ptr *p);
      const image_any& image = raster->data_;
      image.size();
      }|]
    bytes <- mallocForeignPtrBytes numBytes
    (  toEnum . fromIntegral -> dtype
     , fromIntegral -> _rasterWidth
     , fromIntegral -> _rasterHeight
     , _rasterExtent
     , _rasterQueryExtent
     , nodataPtr
     , realToFrac -> _rasterFilterFactor
     ) <- C.withPtrs_ $ \(dtype, w, h, ext, qext, nd, ff) ->
      [C.block|void {
      const raster_ptr &raster = *$(raster_ptr *p);
      const image_any& image = raster->data_;
      *$(unsigned char *dtype) = static_cast<unsigned char>(image.get_dtype());
      *$(int *w) = image.width();
      *$(int *h) = image.height();
      *$(bbox *ext) = raster->ext_;
      *$(bbox *qext) = raster->query_ext_;
      *$(double *ff) = raster->filter_factor_;
      std::memcpy($fptr-ptr:(unsigned char *bytes), image.bytes(), image.size());
      auto nodata = raster->nodata();
      if (nodata) {
        *$(double **nd) = &*nodata;
      } else {
        *$(double **nd) = nullptr;
      }
      }|]
    let sz = _rasterWidth*_rasterHeight
    _rasterNodata <- if nodataPtr==nullPtr
                     then return Nothing
                     else Just . realToFrac <$> peek nodataPtr
    case dtype of
      DTYPE_CASE(DTypeRgba8, PixelRgba8)
      DTYPE_CASE(DTypeGray8, Word8)
      DTYPE_CASE(DTypeGray8s, Int8)
      DTYPE_CASE(DTypeGray16, Word16)
      DTYPE_CASE(DTypeGray16s, Int16)
      DTYPE_CASE(DTypeGray32, Word32)
      DTYPE_CASE(DTypeGray32s, Int32)
      DTYPE_CASE(DTypeGray32f, Float)
      DTYPE_CASE(DTypeGray64, Word64)
      DTYPE_CASE(DTypeGray64s, Int64)
      DTYPE_CASE(DTypeGray64f, Double)
      _ -> throwIO (VariantTypeError "SomeRaster")
