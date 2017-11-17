{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.Image (
  Image
, Size
, unsafeNew
, serialize
, fromRgba8
, toRgba8
) where

import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import           Control.Monad ((<=<))
import           Data.ByteString (ByteString)
import           Data.String (fromString)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

import           System.IO.Unsafe (unsafePerformIO)


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/image.hpp>"
C.include "<mapnik/image_util.hpp>"

C.using "namespace mapnik"
C.using "pixel_rgba8 = std::uint32_t"

-- * Image


foreign import ccall "&hs_mapnik_destroy_Image" destroyImage :: FinalizerPtr Image

unsafeNew :: (Ptr (Ptr Image) -> IO ()) -> IO Image
unsafeNew = fmap Image . newForeignPtr destroyImage <=< C.withPtr_


--TODO: Format should not be stringly typed so we can change signature to:
--      serialize :: Format -> Image -> ByteString
serialize :: String -> Image -> Maybe ByteString
serialize (fromString -> fmt) im = unsafePerformIO $ newByteStringMaybe $ \(ptr, len) ->
  [CU.block|void {
  std::string fmt = std::string($bs-ptr:fmt, $bs-len:fmt);
  *$(char** ptr) = NULL;
  try {
    std::string s = save_to_string(*$fptr-ptr:(image_rgba8 *im), fmt);
    if (s.length() ) {
      *$(int* len) = s.length();
      *$(char** ptr) = static_cast<char*>(malloc(s.length()));
      std::memcpy( *$(char** ptr), s.c_str(), s.length());
    }
  } catch (...) {
    // pass
  }
  }|]

type Size = (Int,Int)


toRgba8 :: Image -> (Size, V.Vector PixelRgba8)
toRgba8 im = unsafePerformIO $ do
  (fromIntegral -> w, fromIntegral -> h) <- C.withPtrs_ $ \(w,h) ->
    [CU.block|void {
    mapnik::image_rgba8 *im = $fptr-ptr:(image_rgba8 *im);
    *$(size_t *w) = im->width();
    *$(size_t *h) = im->height();
    }|]
  vec <- MV.unsafeNew (w*h)
  [CU.block|void {
  mapnik::image_rgba8 *im = $fptr-ptr:(image_rgba8 *im);
  std::memcpy($vec-ptr:(pixel_rgba8 *vec), im->data(), im->size());
  }|]
  (,) <$> pure (w,h) <*> V.unsafeFreeze vec
{-# NOINLINE toRgba8 #-}

fromRgba8 :: (Size, V.Vector PixelRgba8) -> Maybe Image
fromRgba8 ((width, height), rgba8)
  | 0 == V.length rgba8 = Nothing
  | height < 0 || width < 0 = Nothing
  | width*height /= V.length rgba8 = Nothing
fromRgba8 ((fromIntegral -> width, fromIntegral -> height), rgba8) = unsafePerformIO $ fmap Just $ unsafeNew $ \ptr ->
  [CU.block|void {
  *$(image_rgba8 **ptr) =
    new mapnik::image_rgba8(
      $(int width), $(int height),
      $vec-ptr:(pixel_rgba8 *rgba8)
      );
  }|]
{-# NOINLINE fromRgba8 #-}
  
{-# RULES
"fromRgba8/toRgba8"  forall im. fromRgba8 (toRgba8 im) = Just im
  #-}
