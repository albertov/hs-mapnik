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
import           Foreign.Storable

import qualified Language.C.Inline.Cpp as C

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/color.hpp>"
C.include "<mapnik/color_factory.hpp>"

C.using "namespace mapnik"

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
