{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Layer (
  Layer
, unsafeNew
, create
, addStyle
, setDatasource
, setSrs
, setMaxExtent
, setMaxScaleDenominator
, setMinScaleDenominator
, setQueryable
) where

import           Mapnik.Internal
import           Control.Monad ((<=<))
import           Data.String (fromString)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/layer.hpp>"

C.using "namespace mapnik"

--
-- * Layer


foreign import ccall "&hs_mapnik_destroy_Layer" destroyLayer :: FinalizerPtr Layer

unsafeNew :: (Ptr (Ptr Layer) -> IO ()) -> IO Layer
unsafeNew = fmap Layer . newForeignPtr destroyLayer <=< C.withPtr_

create :: String -> IO Layer
create (fromString -> name) =
  unsafeNew $ \p -> [C.catchBlock|*$(layer** p) = new layer(std::string($bs-ptr:name, $bs-len:name));|]

addStyle :: Layer -> String -> IO ()
addStyle l (fromString -> s) = [C.block| void {
  $fptr-ptr:(layer *l)->add_style(std::string($bs-ptr:s, $bs-len:s));
  }|]

setSrs :: Layer -> String -> IO ()
setSrs l (fromString -> srs) =
  [C.block|void { $fptr-ptr:(layer *l)->set_srs(std::string($bs-ptr:srs, $bs-len:srs)); }|]

setMaxScaleDenominator :: Layer -> Double -> IO ()
setMaxScaleDenominator l (realToFrac -> s) =
  [C.block|void { $fptr-ptr:(layer *l)->set_maximum_scale_denominator($(double s)); }|]

setMinScaleDenominator :: Layer -> Double -> IO ()
setMinScaleDenominator l (realToFrac -> s) =
  [C.block|void { $fptr-ptr:(layer *l)->set_minimum_scale_denominator($(double s)); }|]

setQueryable :: Layer -> Bool -> IO ()
setQueryable l (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(layer *l)->set_queryable($(int q)); }|]

setDatasource :: Layer -> Datasource -> IO ()
setDatasource l ds =
  [C.block|void { $fptr-ptr:(layer *l)->set_datasource(*$fptr-ptr:(datasource_ptr *ds)); }|]

setMaxExtent :: Layer -> Box -> IO ()
setMaxExtent l (Box (realToFrac -> x0) (realToFrac -> y0) (realToFrac -> x1) (realToFrac -> y1)) = 
  [C.block|void {
  $fptr-ptr:(layer *l)->set_maximum_extent(mapnik::box2d<double>($(double x0), $(double y0), $(double x1), $(double y1)));
  }|]

