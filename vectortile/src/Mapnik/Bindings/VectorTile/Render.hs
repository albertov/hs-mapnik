{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.VectorTile.Render (
  RenderSettings(..)
, render
, renderSettings
) where

import           Mapnik.Bindings.VectorTile.Types

import           Mapnik.Bindings.Types (Map(..), mapnikCtx)
import           Mapnik.Bindings.Util (newByteString)
import           Mapnik.Bindings (MapnikError)
import qualified Mapnik.Bindings.Cpp as C
import qualified Mapnik.Bindings.Map as Map

import Control.Exception

import GHC.Generics (Generic)

C.context mapnikCtx
C.include "<vector_tile_tile.hpp>"
C.include "<vector_tile_processor.hpp>"
C.using "namespace mapnik"
C.using "namespace mapnik::vector_tile_impl"
C.using "bbox = box2d<double>"

data RenderSettings = RenderSettings
  { tileIndex  :: !XYZ
  , bufferSize :: !Int
  } deriving (Eq, Show, Generic)

renderSettings :: XYZ -> RenderSettings
renderSettings v = RenderSettings v 0

-- | Renders a 'Map.Map' as a MVT encoded 'ByteString'
render :: Map.Map -> RenderSettings -> IO (Either MapnikError MVTile)
render m cfg =
  try $ fmap MVTile $ newByteString $ \(ptr,len) -> [C.catchBlock|
    auto m = *$fptr-ptr:(Map *m);
    const int tile_size = 4096;
    mapnik::vector_tile_impl::processor ren(m);
    merc_tile t($(uint64_t x), $(uint64_t y), $(uint64_t z),
      tile_size, $(int32_t bfSz));
    ren.update_tile(t,0,0);
    *$(char **ptr) = static_cast<char*>(malloc(t.size()));
    *$(int *len) = t.size();
    std::memcpy(*$(char **ptr), t.data(), t.size());
    |]
  where
    RenderSettings { tileIndex = XYZ (fromIntegral -> x)
                                     (fromIntegral -> y)
                                     (fromIntegral -> z)
                   , bufferSize = (fromIntegral -> bfSz)
                   , ..
                   } = cfg
