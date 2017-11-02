{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Image (
  Image
, unsafeNew
, serialize
, fromRgba8
, toRgba8
) where

import           Mapnik.Internal
import           Control.Monad ((<=<))
import           Data.String (fromString)
import           Data.ByteString as BS (ByteString, length)
import           Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)
import           Foreign.C.String (CString)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C

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


newByteString :: ((Ptr CString, Ptr C.CInt) -> IO ()) -> IO ByteString
newByteString block = do
  (ptr,len) <- C.withPtrs_ block
  unsafePackMallocCStringLen (ptr, fromIntegral len)

serialize :: String -> Image -> ByteString
serialize (fromString -> fmt) im = unsafePerformIO $ newByteString $ \(ptr, len) ->
  [C.catchBlock|
  std::string fmt = std::string($bs-ptr:fmt, $bs-len:fmt);
  std::string s = save_to_string(*$fptr-ptr:(image_rgba8 *im), fmt);
  if (! s.length() ) {
    throw std::runtime_error("could not serialize image");
  }
  *$(char** ptr) = static_cast<char*>(malloc(s.length()));
  if ( ! *$(char** ptr) ) {
    throw std::runtime_error("Could not malloc");
  }
  memcpy( *$(char** ptr), s.c_str(), s.length());
  *$(int* len) = s.length();
  |]


toRgba8 :: Image -> ByteString
toRgba8 im = unsafePerformIO $ newByteString $ \(ptr, len) ->
  [C.catchBlock|
  mapnik::image_rgba8 *im = $fptr-ptr:(image_rgba8 *im);
  int len = *$(int* len) = im->size();
  if (len <= 0) {
    throw std::runtime_error("Invalid image size");
  }
  *$(char** ptr) = static_cast<char*>(malloc(len));
  if ( ! *$(char** ptr) ) {
    throw std::runtime_error("Could not malloc");
  }
  memcpy(*$(char** ptr), im->data(), len);
  |]
{-# NOINLINE toRgba8 #-}

fromRgba8 :: Int -> Int -> ByteString -> Maybe Image
fromRgba8 width height rgba8
  | 0 == BS.length rgba8 = Nothing
  | height < 0 || width < 0 = Nothing
  | width*height*4 /= BS.length rgba8 = Nothing
fromRgba8 (fromIntegral -> width) (fromIntegral -> height) rgba8 = unsafePerformIO $ fmap Just $ unsafeNew $ \ptr ->
  [C.block|void {
  *$(image_rgba8 **ptr) =
    new mapnik::image_rgba8($(int width), $(int height), reinterpret_cast<unsigned char*>($bs-ptr:rgba8));
  }|]
{-# NOINLINE fromRgba8 #-}
  
{-# RULES
"fromRgba8/toRgba8"  forall w h im. fromRgba8 w h (toRgba8 im) = Just im
  #-}
