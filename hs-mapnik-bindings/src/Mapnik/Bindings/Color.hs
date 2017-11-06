{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.Color (
  create
, tShowColor
, unsafeNew
, unsafeNewMaybe
, unCreate
, mkCreateMaybe
) where

import qualified Mapnik
import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Data.Maybe (fromJust)
import           Control.Exception (try)
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr)
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
unsafeNew = mkUnsafeNew Color destroyColor

unsafeNewMaybe :: (Ptr (Ptr Color) -> IO ()) -> IO (Maybe Color)
unsafeNewMaybe = mkUnsafeNewMaybe Color destroyColor

create :: Mapnik.Color -> Maybe Color
create col = unsafePerformIO $ case col of
  Mapnik.RGBA (fromIntegral ->r) (fromIntegral -> g) (fromIntegral -> b) (fromIntegral -> a) -> fmap Just $ unsafeNew $ \p ->
    [C.block|void{*$(color** p) = new color($(unsigned char r), $(unsigned char g), $(unsigned char b), $(unsigned char a));}|]
  Mapnik.Color (encodeUtf8 -> c) ->
    fmap fromExc $ try @C.CppException $ unsafeNew $ \p ->
      [C.catchBlock|*$(color** p) = new color(parse_color(std::string($bs-ptr:c, $bs-len:c)));|]
  where
    fromExc = either (const Nothing) Just

mkCreateMaybe :: ((Ptr C.CInt, Ptr C.CUChar, Ptr C.CUChar, Ptr C.CUChar, Ptr C.CUChar) -> IO ()) -> IO (Maybe Mapnik.Color)
mkCreateMaybe fun = do
  (has,fromIntegral->r,fromIntegral->g,fromIntegral->b,fromIntegral->a) <- C.withPtrs_ fun
  return $ if has==1 then Just (Mapnik.RGBA r g b a) else Nothing

unCreate :: Color -> IO Mapnik.Color
unCreate c = fmap fromJust $ mkCreateMaybe $ \(has,r,g,b,a) ->
  [C.block|void {
  color const& c = *$fptr-ptr:(color *c);
  *$(int *has) = 1;
  *$(unsigned char *r) = c.red();
  *$(unsigned char *g) = c.green();
  *$(unsigned char *b) = c.blue();
  *$(unsigned char *a) = c.alpha();
  }|]

instance Show Color where
  show = unpack . tShowColor

tShowColor :: Color -> Text
tShowColor c = unsafePerformIO $ newText $ \(ptr,len) ->
  [C.block|void {
  std::string s = $fptr-ptr:(color *c)->to_string();
  *$(char** ptr)= strdup(s.c_str());
  *$(int* len) = s.length();
  }|]
