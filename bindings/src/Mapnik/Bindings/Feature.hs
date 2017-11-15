{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.Feature (
  Feature (..)
, Geometry (..)
, createFeature
, createRasterFeature
) where

import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Raster
import           Mapnik.Bindings.Variant

import           Control.Exception (bracket)
import           Control.Monad (forM_)
import           Data.ByteString (ByteString)
import           Data.String (IsString(..))
import           Data.Vector (Vector)
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/feature.hpp>"
C.include "<mapnik/feature_factory.hpp>"
C.include "<mapnik/geometry.hpp>"
C.include "<mapnik/wkb.hpp>"
C.include "<mapnik/wkt/wkt_factory.hpp>"
C.include "<mapnik/wkt/wkt_grammar.hpp>"
C.include "<mapnik/wkt/wkt_grammar_impl.hpp>"
C.include "<mapnik/wkt/wkt_generator_grammar.hpp>"
C.include "<mapnik/unicode.hpp>"

-- Mapnik does not export this template instantiation that the inlined
-- from_wkt needs so we instantitate it here
C.verbatim "using iterator_type = std::string::const_iterator;"
C.verbatim "template struct mapnik::wkt::wkt_grammar<iterator_type>;"

C.using "namespace mapnik"
C.using "mapnik::geometry_utils"

data Geometry
  = GeometryWKB !ByteString
  | GeometryWKT !ByteString
  deriving (Eq, Show)

instance IsString Geometry where
  fromString = GeometryWKT . fromString

data Feature = Feature
  { fid      :: !Int
  , geometry :: !Geometry
  , fields   :: !(Vector Value)
  }
  deriving (Eq, Show)

foreign import ccall "&hs_mapnik_destroy_Feature" destroyFeature :: FinalizerPtr FeaturePtr

unsafeNew :: (Ptr (Ptr FeaturePtr) -> IO ()) -> IO FeaturePtr
unsafeNew = mkUnsafeNew FeaturePtr destroyFeature

createFeature :: Ptr FeatureCtx -> Feature -> IO FeaturePtr
createFeature ctx f = unsafeNew $ \ret -> do
  [CU.block|void {
  auto feat = *$(feature_ptr **ret) = new feature_ptr(
    feature_factory::create(*$(context_ptr *ctx), $(value_integer fid'))
    );
  }|]
  case geometry of
    GeometryWKB wkb ->
      [CU.block|void {
      auto feat = **$(feature_ptr **ret);
      feat->set_geometry(geometry_utils::from_wkb($bs-ptr:wkb, $bs-len:wkb, mapnik::wkbAuto));
      }|]
    GeometryWKT wkt ->
      [CU.block|void{
      auto feat = **$(feature_ptr **ret);
      mapnik::geometry::geometry<double> geom;
      if (mapnik::from_wkt(std::string($bs-ptr:wkt, $bs-len:wkt), geom)) {
        feat->set_geometry(std::move(geom));
      }
      }|]
  withVec $ \fs -> do
    forM_ fields $ flip withV $ \fld ->
      [CU.block|void{
        auto fs = static_cast<feature_impl::cont_type*>($(void *fs));
        fs->push_back(*$(value *fld));
      }|]
    [CU.block|void{
      auto feat = **$(feature_ptr **ret);
      auto fs = static_cast<feature_impl::cont_type*>($(void *fs));
      feat->set_data(*fs);
    }|]
  where 
    Feature {fid=(fromIntegral -> fid'),..}  = f

    withVec = bracket alloc dealloc where
      alloc = [CU.exp|void * { new feature_impl::cont_type() }|]
      dealloc p = [CU.exp| void { delete static_cast<feature_impl::cont_type*>($(void *p)) } |]

createRasterFeature
  :: Variant RasterPtr (Raster a)
  => Ptr FeatureCtx -> Raster a -> IO FeaturePtr
createRasterFeature ctx r = unsafeNew $ \ret -> withV r $ \raster ->
  [CU.block|void {
  auto feat = *$(feature_ptr **ret) = new feature_ptr(
    feature_factory::create(*$(context_ptr *ctx), 0)
    );
  feat->get()->set_raster(*$(raster_ptr *raster));
  }|]
