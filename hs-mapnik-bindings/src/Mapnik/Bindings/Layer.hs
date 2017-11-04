{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.Layer (
  Layer
, unsafeNew
, unsafeNewMaybe
, create
, addStyle
, setDatasource
, setSrs
, setBufferSize
, setMaxExtent
, setMaxScaleDenominator
, setMinScaleDenominator
, setQueryable
, setGroupBy
, setClearLabelCache
, setCacheFeatures
) where

import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Datasource ()
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/layer.hpp>"

C.using "namespace mapnik"

--
-- * Layer


foreign import ccall "&hs_mapnik_destroy_Layer" destroyLayer :: FinalizerPtr Layer

unsafeNew :: (Ptr (Ptr Layer) -> IO ()) -> IO Layer
unsafeNew = mkUnsafeNew Layer destroyLayer

unsafeNewMaybe :: (Ptr (Ptr Layer) -> IO ()) -> IO (Maybe Layer)
unsafeNewMaybe = mkUnsafeNewMaybe Layer destroyLayer

create :: Text -> IO Layer
create (encodeUtf8 -> name) =
  unsafeNew $ \p -> [C.block|void {*$(layer** p) = new layer(std::string($bs-ptr:name, $bs-len:name));}|]

addStyle :: Layer -> Text -> IO ()
addStyle l (encodeUtf8 -> s) = [C.block| void {
  $fptr-ptr:(layer *l)->add_style(std::string($bs-ptr:s, $bs-len:s));
  }|]

setGroupBy :: Layer -> Text -> IO ()
setGroupBy l (encodeUtf8 -> srs) =
  [C.block|void { $fptr-ptr:(layer *l)->set_group_by(std::string($bs-ptr:srs, $bs-len:srs)); }|]

setSrs :: Layer -> Text -> IO ()
setSrs l (encodeUtf8 -> srs) =
  [C.block|void { $fptr-ptr:(layer *l)->set_srs(std::string($bs-ptr:srs, $bs-len:srs)); }|]

setBufferSize :: Layer -> Int -> IO ()
setBufferSize l (fromIntegral -> s) =
  [C.block|void { $fptr-ptr:(layer *l)->set_buffer_size($(int s)); }|]

setMaxScaleDenominator :: Layer -> Double -> IO ()
setMaxScaleDenominator l (realToFrac -> s) =
  [C.block|void { $fptr-ptr:(layer *l)->set_maximum_scale_denominator($(double s)); }|]

setMinScaleDenominator :: Layer -> Double -> IO ()
setMinScaleDenominator l (realToFrac -> s) =
  [C.block|void { $fptr-ptr:(layer *l)->set_minimum_scale_denominator($(double s)); }|]

setQueryable :: Layer -> Bool -> IO ()
setQueryable l (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(layer *l)->set_queryable($(int q)); }|]

setClearLabelCache :: Layer -> Bool -> IO ()
setClearLabelCache l (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(layer *l)->set_clear_label_cache($(int q)); }|]

setCacheFeatures :: Layer -> Bool -> IO ()
setCacheFeatures l (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(layer *l)->set_cache_features($(int q)); }|]

setDatasource :: Layer -> Datasource -> IO ()
setDatasource l ds =
  [C.block|void { $fptr-ptr:(layer *l)->set_datasource(*$fptr-ptr:(datasource_ptr *ds)); }|]

setMaxExtent :: Layer -> Box -> IO ()
setMaxExtent l (Box (realToFrac -> x0) (realToFrac -> y0) (realToFrac -> x1) (realToFrac -> y1)) = 
  [C.block|void {
  $fptr-ptr:(layer *l)->set_maximum_extent(mapnik::box2d<double>($(double x0), $(double y0), $(double x1), $(double y1)));
  }|]
