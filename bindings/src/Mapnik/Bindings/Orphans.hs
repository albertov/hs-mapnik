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
import qualified Language.C.Inline.Unsafe as CU

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/color.hpp>"
C.include "<mapnik/box2d.hpp>"
C.include "<mapnik/color_factory.hpp>"

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
