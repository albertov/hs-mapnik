{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.Orphans (
) where

import qualified Mapnik
import           Mapnik.Bindings
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.Storable

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/color.hpp>"
C.include "<mapnik/color_factory.hpp>"

C.using "namespace mapnik"

instance Storable Mapnik.Color where
  sizeOf _ = fromIntegral [C.pure|size_t { sizeof(color) }|]
  alignment _ = fromIntegral [C.pure|size_t { alignof (color) }|]
  peek p = do
    (   fromIntegral -> red
      , fromIntegral -> green
      , fromIntegral -> blue
      , fromIntegral -> alpha
      ) <- C.withPtrs_ $ \(r,g,b,a) -> [C.block|void {
      color const& c = *$(color *p);
      *$(unsigned char *r) = c.red();
      *$(unsigned char *g) = c.green();
      *$(unsigned char *b) = c.blue();
      *$(unsigned char *a) = c.alpha();
      }|]
    return Mapnik.RGBA {..}

  poke p (Mapnik.RGBA (fromIntegral ->r) (fromIntegral -> g) (fromIntegral -> b) (fromIntegral -> a)) =
    [C.block|void{
      *$(color* p) = color($(unsigned char r), $(unsigned char g), $(unsigned char b), $(unsigned char a));
      }|]
  poke p (Mapnik.ColorName (encodeUtf8 -> c)) =
    [C.catchBlock|*$(color* p) = parse_color(std::string($bs-ptr:c, $bs-len:c));|]
