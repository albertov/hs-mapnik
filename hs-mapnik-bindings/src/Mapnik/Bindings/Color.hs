{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Mapnik.Bindings.Color (
  create
, bsShowColor
) where

import qualified Mapnik
import           Mapnik.Bindings
import           Mapnik.Bindings.Util (newByteString)
import           Data.ByteString (ByteString, unpack)
import           Data.Char (chr)
import           Control.Exception (try)
import           Control.Monad ((<=<))
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C

import           System.IO.Unsafe (unsafePerformIO)

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/color.hpp>"
C.include "<mapnik/color_factory.hpp>"

C.using "namespace mapnik"

foreign import ccall "&hs_mapnik_destroy_Color" destroyColor :: FinalizerPtr Color

unsafeNew :: (Ptr (Ptr Color) -> IO ()) -> IO Color
unsafeNew = fmap Color . newForeignPtr destroyColor <=< C.withPtr_

create :: Mapnik.Color -> Maybe Color
create col = unsafePerformIO $ case col of
  Mapnik.RGBA (fromIntegral ->r) (fromIntegral -> g) (fromIntegral -> b) (fromIntegral -> a) -> fmap Just $ unsafeNew $ \p ->
    [C.block|void{*$(color** p) = new color($(unsigned char r), $(unsigned char g), $(unsigned char b), $(unsigned char a));}|]
  Mapnik.Color (encodeUtf8 -> c) ->
    fmap fromExc $ try @C.CppException $ unsafeNew $ \p ->
      [C.catchBlock|*$(color** p) = new color(parse_color(std::string($bs-ptr:c, $bs-len:c)));|]
  where
    fromExc = either (const Nothing) Just

instance Show Color where
  show = map (chr . fromIntegral) . unpack . bsShowColor

bsShowColor :: Color -> ByteString
bsShowColor c = unsafePerformIO $ newByteString $ \(ptr,len) ->
    [C.catchBlock|
    std::string s = $fptr-ptr:(color *c)->to_string();
    *$(char** ptr) = static_cast<char*>(malloc(s.length()));
    if ( ! *$(char** ptr) ) {
      throw std::runtime_error("Could not malloc");
    }
    memcpy( *$(char** ptr), s.c_str(), s.length());
    *$(int* len) = s.length();
    |]
