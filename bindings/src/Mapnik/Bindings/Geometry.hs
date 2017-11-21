{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Mapnik.Bindings.Geometry (
  fromWkt
, fromWkb
, toWkt
, toWkb
, unsafeNew
, unsafeNewMaybe
) where

import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import qualified Mapnik.Bindings.Cpp as C

import           Data.ByteString (ByteString, unpack)
import           Data.Char (chr)
import           Data.String
import           Data.Maybe (fromMaybe)
import           Foreign.Ptr (Ptr)
import           Foreign.ForeignPtr (FinalizerPtr)
import           System.IO.Unsafe (unsafePerformIO)

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/geometry.hpp>"
C.include "<mapnik/geometry_is_empty.hpp>"
C.include "<mapnik/wkb.hpp>"
C.include "<mapnik/wkt/wkt_factory.hpp>"
C.include "<mapnik/wkt/wkt_grammar.hpp>"
C.include "<mapnik/wkt/wkt_generator_grammar.hpp>"
C.include "<mapnik/util/geometry_to_wkt.hpp>"
C.include "util.hpp"

C.using "namespace mapnik"
C.using "mapnik::geometry_utils"
C.using "geometry_t = geometry::geometry<double>"


foreign import ccall "&hs_mapnik_destroy_Geometry" destroyGeometry :: FinalizerPtr Geometry

unsafeNew :: (Ptr (Ptr Geometry) -> IO ()) -> IO Geometry
unsafeNew = mkUnsafeNew Geometry destroyGeometry

unsafeNewMaybe :: (Ptr (Ptr Geometry) -> IO ()) -> IO (Maybe Geometry)
unsafeNewMaybe = mkUnsafeNewMaybe Geometry destroyGeometry

fromWkb :: ByteString -> Maybe Geometry
fromWkb wkb = unsafePerformIO $ unsafeNewMaybe $ \p ->
  [C.block|void{
  geometry_t geom = geometry_utils::from_wkb($bs-ptr:wkb, $bs-len:wkb, mapnik::wkbAuto);
  if (! geometry::is_empty(geom) ) {
    *$(geometry_t **p) = new geometry_t(geom);
  } else {
    *$(geometry_t **p) = nullptr;
  }
  }|]

fromWkt :: ByteString -> Maybe Geometry
fromWkt wkt = unsafePerformIO $ unsafeNewMaybe $ \p ->
  [C.block|void{
  geometry_t geom;
  if (from_wkt(std::string($bs-ptr:wkt, $bs-len:wkt), geom)) {
    *$(geometry_t **p) = new geometry_t(geom);
  } else {
    *$(geometry_t **p) = nullptr;
  }
  }|]

instance Show Geometry where
  show = map (chr.fromIntegral) . unpack . toWkt

instance IsString Geometry where
  fromString = fromMaybe (error "Invalid WKT geometry") . fromWkt . fromString

toWkb :: Geometry -> ByteString
toWkb _TODO = undefined

toWkt :: Geometry -> ByteString
toWkt g = unsafePerformIO $ newByteString $ \(p, len) ->
  [C.block|void{
  std::string result;
  if (util::to_wkt(result, *$fptr-ptr:(geometry_t *g))) {
    mallocedString(result, $(char **p), $(int *len));
  } else {
    *$(char **p) = nullptr;
  }
  }|]
