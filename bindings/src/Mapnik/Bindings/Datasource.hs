{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Mapnik.Bindings.Datasource (
  Datasource
, HsDatasource (..)
, Pair (..)
, Query (..)
, Geometry (..)
, Feature (..)
, Parameters
, unsafeNew
, unsafeNewMaybe
, getParameters
, create
, createHsDatasource
, fromList
, toList
, module X
) where

import           Mapnik.Parameter as X (Value(..), Parameter, (.=))
import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Orphans ()
import           Mapnik.Bindings.Feature
import           Mapnik.Bindings.Variant
import           Mapnik.Bindings.Raster

import           System.IO
import           Control.Exception (catch, SomeException)
import           Data.Vector (Vector)
import           Control.Monad (forM_)
import           Data.ByteString (packCString)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.IORef
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)
import           Foreign.C.String (CString)
import           Foreign.Marshal.Utils (with)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.HashMap.Strict as M
import qualified GHC.Exts as Exts

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Language.C.Inline.Unsafe as CU


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/attribute.hpp>"
C.include "<mapnik/params.hpp>"
C.include "<mapnik/datasource.hpp>"
C.include "<mapnik/datasource_cache.hpp>"
C.include "hs_datasource.hpp"

C.using "namespace mapnik"
C.using "feature_list = hs_featureset::feature_list"
C.using "bbox = box2d<double>"

foreign import ccall "&hs_mapnik_destroy_Parameters" destroyParameters :: FinalizerPtr Parameters
foreign import ccall "&hs_mapnik_destroy_Datasource" destroyDatasource :: FinalizerPtr Datasource

unsafeNew :: (Ptr (Ptr Datasource) -> IO ()) -> IO Datasource
unsafeNew = mkUnsafeNew Datasource destroyDatasource

unsafeNewMaybe :: (Ptr (Ptr Datasource) -> IO ()) -> IO (Maybe Datasource)
unsafeNewMaybe = mkUnsafeNewMaybe Datasource destroyDatasource

create :: Parameters -> IO Datasource
create params = unsafeNew $ \ ptr ->
  [C.catchBlock|
  datasource_ptr p = datasource_cache::instance().create(*$fptr-ptr:(parameters *params));
  *$(datasource_ptr** ptr) = new datasource_ptr(p);
  |]



unsafeNewValues :: (Ptr (Ptr Parameters) -> IO ()) -> IO Parameters
unsafeNewValues = mkUnsafeNew Parameters destroyParameters


getParameters :: Datasource -> IO Parameters
getParameters ds = unsafeNewValues $ \ ptr ->
  [CU.block|void{
  *$(parameters** ptr) = new parameters((*$fptr-ptr:(datasource_ptr *ds))->params());
  }|]

instance Exts.IsList Parameters where
  type Item Parameters = Parameter
  fromList = fromList
  toList = toList

fromList :: [(Text,Value)] -> Parameters
fromList ps = unsafePerformIO $ do
  p <- emptyValues
  forM_ ps $ \(encodeUtf8 -> k, value) -> withV value $ \v ->
    [CU.block|void {
      std::string k($bs-ptr:k, $bs-len:k);
      (*$fptr-ptr:(parameters *p))[k] = *$(value_holder *v);
    }|]
  return p

emptyValues :: IO Parameters
emptyValues = fmap Parameters . newForeignPtr destroyParameters =<< [CU.exp|parameters *{ new parameters }|]

toList :: Parameters -> [(Text,Value)]
toList p = unsafePerformIO $ do
  ref <- newIORef []
  let cb :: CString -> Ptr Param -> IO ()
      cb k v = do
        key <- decodeUtf8 <$> packCString k
        val <- peekV v
        modifyIORef' ref ((key,val):)
  [C.block|void {
     for (parameters::const_iterator it = $fptr-ptr:(parameters *p)->begin();
          it != $fptr-ptr:(parameters *p)->end();
          ++it)
     {
       $fun:(void (*cb)(char*, value_holder*))(
          const_cast<char*>(it->first.c_str()),
          const_cast<value_holder*>(&it->second)
          );
     }
  }|]
  readIORef ref


data Query = Query
  { box              :: !Box
  , unBufferedBox    :: !Box
  , resolution       :: !Pair
  , scaleDenominator :: !Double
  , filterFactor     :: !Double
  , variables        :: !Attributes
  }
  deriving (Eq, Show)

data Pair = Pair { x, y :: !Double }
  deriving (Eq, Show)

data HsDatasource where
  HsVector ::
    { name               :: !Text
    , extent             :: !Box
    , fieldNames         :: !(Vector Text)
    , getFeatures        :: !(Query -> IO [Feature])
    , getFeaturesAtPoint :: !(Pair -> Double -> IO [Feature])
    } -> HsDatasource
  HsRaster :: Variant RasterPtr (Raster a) =>
    { name               :: !Text
    , extent             :: !Box
    , fieldNames         :: !(Vector Text)
    , getRaster          :: !(Query -> IO (Raster a))
    , getFeaturesAtPoint :: !(Pair -> Double -> IO [Feature])
    } -> HsDatasource



createHsDatasource :: HsDatasource -> IO Datasource
createHsDatasource HsVector{..} = with extent $ \e -> unsafeNew $ \ ptr -> do
  fs <- $(C.mkFunPtr [t|Ptr FeatureCtx -> Ptr FeatureList -> Ptr QueryPtr -> IO ()|]) getFeatures'
  fsp <- $(C.mkFunPtr [t|Ptr FeatureCtx -> Ptr FeatureList -> C.CDouble -> C.CDouble -> C.CDouble -> IO ()|]) getFeaturesAtPoint'
  [CU.block|void {
  datasource_ptr p = std::make_shared<hs_datasource>(
    "hs_vector_layer",                 //TODO
    datasource::Vector,                //TODO
    datasource_geometry_t::Collection, //TODO
    *$(bbox *e),
    $(features_callback fs),
    $(features_at_point_callback fsp)
    );
  *$(datasource_ptr** ptr) = new datasource_ptr(p);
  }|]
  forM_ fieldNames $ \(encodeUtf8 -> v) ->
    [CU.block|void {
      auto ds = dynamic_cast<hs_datasource*>((*$(datasource_ptr **ptr))->get());
      ds->push_key(std::string($bs-ptr:v, $bs-len:v));
    }|]

  where
    getFeatures' ctx fs q = catchingExceptions "getFeatures" $
      mapM_ (pushBack ctx fs) =<< getFeatures =<< unCreateQuery q

    getFeaturesAtPoint' ctx fs (realToFrac -> x) (realToFrac -> y) (realToFrac -> tol) =
      catchingExceptions "getFeaturesAtPoint" $
        mapM_ (pushBack ctx fs) =<< getFeaturesAtPoint (Pair x y) tol

createHsDatasource HsRaster{..} = with extent $ \e -> unsafeNew $ \ ptr -> do
  fs <- $(C.mkFunPtr [t|Ptr FeatureCtx -> Ptr FeatureList -> Ptr QueryPtr -> IO ()|]) getFeatures'
  fsp <- $(C.mkFunPtr [t|Ptr FeatureCtx -> Ptr FeatureList -> C.CDouble -> C.CDouble -> C.CDouble -> IO ()|]) getFeaturesAtPoint'
  [CU.block|void {
  datasource_ptr p = std::make_shared<hs_datasource>(
    "hs_raster_layer",                 //TODO
    datasource::Raster,                //TODO
    boost::optional<mapnik::datasource_geometry_t>(),
    *$(bbox *e),
    $(features_callback fs),
    $(features_at_point_callback fsp)
    );
  *$(datasource_ptr** ptr) = new datasource_ptr(p);
  }|]
  forM_ fieldNames $ \(encodeUtf8 -> v) ->
    [CU.block|void {
      auto ds = dynamic_cast<hs_datasource*>((*$(datasource_ptr **ptr))->get());
      ds->push_key(std::string($bs-ptr:v, $bs-len:v));
    }|]

  where
    getFeatures' ctx fs q = catchingExceptions "getRaster" $ do
      f <- createRasterFeature ctx =<< getRaster =<< unCreateQuery q
      [CU.block|void {
        $(feature_list *fs)->push_back(*$fptr-ptr:(feature_ptr *f)); }
      |]

    getFeaturesAtPoint' ctx fs (realToFrac -> x) (realToFrac -> y) (realToFrac -> tol) =
      catchingExceptions "getFeaturesAtPoint" $
        mapM_ (pushBack ctx fs) =<< getFeaturesAtPoint (Pair x y) tol

pushBack :: Ptr FeatureCtx -> Ptr FeatureList -> Feature -> IO ()
pushBack ctx fs = \f -> do
  f' <- createFeature ctx f
  [CU.block|void {
    $(feature_list *fs)->push_back(*$fptr-ptr:(feature_ptr *f')); }
  |]


catchingExceptions :: String -> IO () -> IO ()
catchingExceptions ctx = (`catch` (\e ->
  hPutStrLn stderr ("Unhandled haskell exception in " ++ ctx ++ ": " ++ show @SomeException e)))


unCreateQuery :: Ptr QueryPtr -> IO Query
unCreateQuery q = do
  (   realToFrac -> resx
    , realToFrac -> resy
    , realToFrac -> scaleDenominator
    , realToFrac -> filterFactor
    , box
    , unBufferedBox
    , varPtr
    ) <- C.withPtrs_ $ \(x,y,s,f,b,ub,vs) ->
    [CU.block|void{
    const query& q     = *$(query *q);
    auto res           = q.resolution();
    *$(double *x)      = std::get<0>(res);
    *$(double *y)      = std::get<1>(res);
    *$(double *s)      = q.scale_denominator();
    *$(double *f)      = q.get_filter_factor();
    *$(bbox *b)        = q.get_bbox();
    *$(bbox *ub)       = q.get_unbuffered_bbox();
    *$(attributes **vs) = const_cast<attributes*>(&q.variables());
    }|]
  variables <- extractAttributes varPtr
  return Query{resolution=Pair resx resy, ..}

extractAttributes :: Ptr Attributes -> IO Attributes
extractAttributes p = do
  ref <- newIORef M.empty
  let cb :: CString -> Ptr Value -> IO ()
      cb k v = do
        key <- decodeUtf8 <$> packCString k
        val <- peekV v
        modifyIORef' ref (M.insert key val)
  [C.block|void {
     for (attributes::const_iterator it = $(attributes *p)->begin();
          it != $(attributes *p)->end();
          ++it)
     {
       $fun:(void (*cb)(char*, value*))(
          const_cast<char*>(it->first.c_str()),
          const_cast<value*>(&it->second)
          );
     }
  }|]
  readIORef ref
