{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.Image (
  Image
, unsafeNew
, serialize
, fromRgba8
, toRgba8
) where

import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Control.Monad ((<=<))
import           Data.String (fromString)
import           Data.ByteString as BS (ByteString, length)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C

import           System.IO.Unsafe (unsafePerformIO)


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/image.hpp>"
C.include "<mapnik/image_util.hpp>"

C.using "namespace mapnik"

-- * Image


foreign import ccall "&hs_mapnik_destroy_Image" destroyImage :: FinalizerPtr Image

unsafeNew :: (Ptr (Ptr Image) -> IO ()) -> IO Image
unsafeNew = fmap Image . newForeignPtr destroyImage <=< C.withPtr_


--TODO: Format should not be stringly typed so we can change signature to:
--      serialize :: Format -> Image -> ByteString
serialize :: String -> Image -> Maybe ByteString
serialize (fromString -> fmt) im = unsafePerformIO $ newByteStringMaybe $ \(ptr, len) ->
  [C.block|void {
  std::string fmt = std::string($bs-ptr:fmt, $bs-len:fmt);
  *$(char** ptr) = NULL;
  try {
    std::string s = save_to_string(*$fptr-ptr:(image_rgba8 *im), fmt);
    if (s.length() ) {
      *$(int* len) = s.length();
      *$(char** ptr) = static_cast<char*>(malloc(s.length()));
      memcpy( *$(char** ptr), s.c_str(), s.length());
    }
  } catch (...) {
    // pass
  }
  }|]

type Size = (Int,Int)


toRgba8 :: Image -> (Size, ByteString)
toRgba8 im = unsafePerformIO $ do
  ((fromIntegral -> w, fromIntegral -> h), bs) <- C.withPtrs $ \(w,h) ->
    newByteString $ \(ptr, len) ->
      [C.block|void {
      static char empty='\0';
      mapnik::image_rgba8 *im = $fptr-ptr:(image_rgba8 *im);
      int len = *$(int* len) = im->size();
      *$(size_t *w) = im->width();
      *$(size_t *h) = im->height();
      if (len > 0) {
        *$(char** ptr) = static_cast<char*>(malloc(len));
        memcpy(*$(char** ptr), im->data(), len);
      } else {
        *$(char** ptr) = &empty;
      }
      }|]
  return ((w,h), bs)
{-# NOINLINE toRgba8 #-}

fromRgba8 :: (Size, ByteString) -> Maybe Image
fromRgba8 ((width, height), rgba8)
  | 0 == BS.length rgba8 = Nothing
  | height < 0 || width < 0 = Nothing
  | width*height*4 /= BS.length rgba8 = Nothing
fromRgba8 ((fromIntegral -> width, fromIntegral -> height), rgba8) = unsafePerformIO $ fmap Just $ unsafeNew $ \ptr ->
  [C.block|void {
  *$(image_rgba8 **ptr) =
    new mapnik::image_rgba8($(int width), $(int height), reinterpret_cast<unsigned char*>($bs-ptr:rgba8));
  }|]
{-# NOINLINE fromRgba8 #-}
  
{-# RULES
"fromRgba8/toRgba8"  forall im. fromRgba8 (toRgba8 im) = Just im
  #-}
