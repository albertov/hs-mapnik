{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.Feature (
  Feature (..)
, Geometry (..)
, feature
, create
, unCreate
) where

import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Raster
import           Mapnik.Bindings.Variant
import qualified Mapnik.Bindings.Geometry as Geometry

import           Control.Exception (bracket)
import           Control.Monad (forM_)
import           Data.IORef
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/geometry.hpp>"
C.include "<mapnik/geometry_is_empty.hpp>"
C.include "<mapnik/raster.hpp>"
C.include "<mapnik/feature.hpp>"
C.include "<mapnik/feature_factory.hpp>"
C.include "<mapnik/unicode.hpp>"


C.using "namespace mapnik"
C.using "geometry_t = geometry::geometry<double>"

data Feature = Feature
  { fid      :: !Int
  , geometry :: !(Maybe Geometry)
  , fields   :: !(Vector Value)
  , raster   :: !(Maybe SomeRaster)
  }
  deriving (Eq, Show)

feature :: Feature
feature = Feature 0 Nothing V.empty Nothing

foreign import ccall "&hs_mapnik_destroy_Feature" destroyFeature :: FinalizerPtr FeaturePtr

unsafeNew :: (Ptr (Ptr FeaturePtr) -> IO ()) -> IO FeaturePtr
unsafeNew = mkUnsafeNew FeaturePtr destroyFeature

create :: Ptr FeatureCtx -> Feature -> IO FeaturePtr
create ctx f = unsafeNew $ \ret -> do
  [CU.block|void {
  auto feat = *$(feature_ptr **ret) = new feature_ptr(
    feature_factory::create(*$(context_ptr *ctx), $(value_integer fid'))
    );
  }|]
  forM_ geometry $ \g ->
    [CU.block|void{
      auto feat = **$(feature_ptr **ret);
      feat->set_geometry_copy(*$fptr-ptr:(geometry_t *g));
    }|]
  forM_ raster $ \(SomeRaster r) -> withV r $ \r'->
    [CU.block|void{
      auto feat = **$(feature_ptr **ret);
      feat->set_raster(*$(raster_ptr *r'));
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

unCreate :: Ptr FeaturePtr -> IO Feature
unCreate p = do
  fid <- fromIntegral <$> [CU.exp|int { (*$(feature_ptr* p))->id() }|]
  geometry <- Geometry.unsafeNewMaybe $ \g ->
    [CU.block|void {
    auto feat = *$(feature_ptr *p);
    if (! geometry::is_empty(feat->get_geometry()) ) {
      *$(geometry_t **g) = new geometry_t(feat->get_geometry());
    } else {
      *$(geometry_t **g) = nullptr;
    }
    }|]
  fieldsRef <- newIORef []
  let cb :: Ptr Value -> IO ()
      cb v = peekV v >>= \f -> modifyIORef' fieldsRef (f:)
  [C.block|void {
      auto fs = (*$(feature_ptr *p))->get_data();
      for (auto it=fs.begin(); it!=fs.end(); ++it) {
        $fun:(void (*cb)(value *))(&*it);
      }
  }|]
  fields <- V.reverse . V.fromList <$> readIORef fieldsRef
  raster <- allocaV $ \ret -> do
    has <- C.withPtr_ $ \has ->
      [CU.block|void {
      auto feat = *$(feature_ptr *p);
      *$(raster_ptr *ret) = feat->get_raster();
      *$(int *has) = *$(raster_ptr *ret)? 1 : 0;
      }|]
    if has==1 then Just <$> peekV ret else return Nothing
  return Feature{..}
