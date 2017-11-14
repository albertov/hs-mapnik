{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
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

import           Mapnik.Parameter as X (ParamValue(..), Parameter, (.=))
import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Orphans ()
import           Mapnik.Bindings.Feature

import           System.IO
import           Control.Exception (throwIO, catch, SomeException)
import           Data.Vector (Vector)
import           Control.Monad (forM_)
import           Data.ByteString.Unsafe (unsafePackMallocCString)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.IORef
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.Storable
import           Foreign.C.String (CString)
import           Foreign.Marshal.Utils (with)
import           System.IO.Unsafe (unsafePerformIO)
import qualified GHC.Exts as Exts

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import qualified Language.C.Inline.Unsafe as CU


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/params.hpp>"
C.include "<mapnik/datasource.hpp>"
C.include "<mapnik/datasource_cache.hpp>"
C.include "parameter_util.hpp"
C.include "hs_datasource.hpp"

C.using "namespace mapnik"
C.verbatim "typedef hs_featureset::feature_list feature_list;"
C.verbatim "typedef box2d<double> bbox;"

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



unsafeNewParams :: (Ptr (Ptr Parameters) -> IO ()) -> IO Parameters
unsafeNewParams = mkUnsafeNew Parameters destroyParameters


getParameters :: Datasource -> IO Parameters
getParameters ds = unsafeNewParams $ \ ptr ->
  [CU.block|void{
  *$(parameters** ptr) = new parameters((*$fptr-ptr:(datasource_ptr *ds))->params());
  }|]

instance Exts.IsList Parameters where
  type Item Parameters = Parameter
  fromList = fromList
  toList = toList

fromList :: [(Text,ParamValue)] -> Parameters
fromList ps = unsafePerformIO $ do
  p <- emptyParams
  forM_ ps $ \(encodeUtf8 -> k, value) ->
    case value of
      StringParam (encodeUtf8 -> v) ->
        [CU.block|void {
          std::string k($bs-ptr:k, $bs-len:k);
          std::string v($bs-ptr:v, $bs-len:v);
          (*$fptr-ptr:(parameters *p))[k] = value_holder(v);
        }|]
      DoubleParam (realToFrac -> v) ->
        [CU.block|void {
          std::string k($bs-ptr:k, $bs-len:k);
          (*$fptr-ptr:(parameters *p))[k] = value_holder($(double v));
        }|]
      IntParam (fromIntegral -> v) ->
        [CU.block|void {
          std::string k($bs-ptr:k, $bs-len:k);
          (*$fptr-ptr:(parameters *p))[k] = value_holder($(value_integer v));
        }|]
      BoolParam (fromIntegral . fromEnum -> v) ->
        [CU.block|void {
          std::string k($bs-ptr:k, $bs-len:k);
          (*$fptr-ptr:(parameters *p))[k] = value_holder($(int v)?true:false);
        }|]
      NullParam ->
        [CU.block|void {
          std::string k($bs-ptr:k, $bs-len:k);
          (*$fptr-ptr:(parameters *p))[k] = value_null();
        }|]
  return p

emptyParams :: IO Parameters
emptyParams = fmap Parameters . newForeignPtr destroyParameters =<< [CU.exp|parameters *{ new parameters }|]


toList :: Parameters -> [(Text,ParamValue)]
toList ps = unsafePerformIO $ do
  resultRef <- newIORef []
  let callback :: C.CInt -> Ptr () -> CString -> IO ()
      callback 0 _ kptr = do
        k <- decodeUtf8 <$> unsafePackMallocCString kptr
        modifyIORef' resultRef ((k, NullParam):)
      callback 1 (castPtr -> (ptr :: Ptr C.CDouble)) kptr = do
        k <- decodeUtf8 <$> unsafePackMallocCString kptr
        v <- realToFrac <$> peek ptr
        modifyIORef' resultRef ((k, DoubleParam v):)
      callback 2 (castPtr -> (ptr :: Ptr MapnikInt)) kptr = do
        k <- decodeUtf8 <$> unsafePackMallocCString kptr
        v <- fromIntegral <$> peek ptr
        modifyIORef' resultRef ((k, IntParam v):)
      callback 3 (castPtr -> (ptr :: Ptr C.CInt)) kptr = do
        k <- decodeUtf8 <$> unsafePackMallocCString kptr
        v <- toEnum . fromIntegral <$> peek ptr
        modifyIORef' resultRef ((k, BoolParam v):)
      callback 4 (castPtr -> (ptr :: CString)) kptr = do
        k <- decodeUtf8 <$> unsafePackMallocCString kptr
        v <- decodeUtf8 <$> unsafePackMallocCString ptr
        modifyIORef' resultRef ((k, StringParam v):)
      callback _ _ kptr = do
        k <- decodeUtf8 <$> unsafePackMallocCString kptr
        throwIO (userError ("Invalid type for parameter at key: " ++ show k))
  [C.block|void {
     for (parameters::const_iterator it = $fptr-ptr:(parameters *ps)->begin();
          it != $fptr-ptr:(parameters *ps)->end();
          ++it)
     {
       util::apply_visitor(value_extractor_visitor($fun:(void (*callback)(param_type , void*, char*)), strdup(it->first.c_str())),it->second);
     }
  }|]
  readIORef resultRef


data Query = Query
  { box              :: !Box
  , unbufferedBox    :: !Box
  , resolution       :: !Pair
  , scaleDenominator :: !Double
  , filterFactor     :: !Double
  }
  deriving (Eq, Show)

data Pair = Pair { x, y :: !Double }
  deriving (Eq, Show)

data HsDatasource = HsVector
  { name               :: !Text
  , extent             :: !Box
  , fieldNames         :: !(Vector Text)
  , getFeatures        :: !(Query -> IO [Feature])
  , getFeaturesAtPoint :: !(Pair -> Double -> IO [Feature])
  }



createHsDatasource :: HsDatasource -> IO Datasource
createHsDatasource HsVector{..} = with extent $ \e -> unsafeNew $ \ ptr -> do
  fs <- $(C.mkFunPtr [t|Ptr FeatureCtx -> Ptr FeatureList -> Ptr QueryPtr -> IO ()|]) getFeatures'
  fsp <- $(C.mkFunPtr [t|Ptr FeatureCtx -> Ptr FeatureList -> C.CDouble -> C.CDouble -> C.CDouble -> IO ()|]) getFeaturesAtPoint'
  [CU.block|void {
  datasource_ptr p = std::make_shared<hs_datasource>(
    "hs_layer",                        //TODO
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

    pushBack ctx fs = \f -> do
      f' <- createFeature ctx f
      [CU.block|void {
        $(feature_list *fs)->push_back(*$fptr-ptr:(feature_ptr *f')); }
      |]

    catchingExceptions ctx = (`catch` (\e ->
      hPutStrLn stderr ("Unhandled haskell exception in " ++ ctx ++ ": " ++ show @SomeException e)))


unCreateQuery :: Ptr QueryPtr -> IO Query
unCreateQuery q = do
  (   realToFrac -> resx
    , realToFrac -> resy
    , realToFrac -> scaleDenominator
    , realToFrac -> filterFactor
    , box
    , unbufferedBox
    ) <- C.withPtrs_ $ \(x,y,s,f,b,ub) ->
    [CU.block|void{
    const query& q = *$(query *q);
    auto res       = q.resolution();
    *$(double *x)  = std::get<0>(res);
    *$(double *y)  = std::get<1>(res);
    *$(double *s)  = q.scale_denominator();
    *$(double *f)  = q.get_filter_factor();
    *$(bbox *b)    = q.get_bbox();
    *$(bbox *ub)   = q.get_unbuffered_bbox();
    }|]
  return Query{resolution=Pair resx resy, ..}
