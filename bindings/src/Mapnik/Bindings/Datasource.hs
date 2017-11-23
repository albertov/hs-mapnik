{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
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
, unsafeNewParameters
, create
, createHsDatasource
, paramsFromMap
, paramsToMap
, features
, featuresAtPoint
, queryBox
, queryBoxProps
, HasBox (..)
, HasUnBufferedBox (..)
, HasResolution (..)
, HasScaleDenominator (..)
, HasVariables (..)
, module X
) where

import qualified Mapnik
import           Mapnik.Lens
import           Mapnik.Parameter as X (Value(..), Parameter, (.=))
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Orphans ()
import           Mapnik.Bindings.Feature (Feature(..), feature)
import qualified Mapnik.Bindings.Feature as Feature
import           Mapnik.Bindings.Variant
import           Mapnik.Bindings.Raster

import           Control.Lens
import           Control.Exception (bracket)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Control.Monad (forM_)
import           Data.ByteString (packCStringLen)
import           Data.Text (Text)
import           Data.List (sortBy)
import           Data.Function (on)
import           Data.Text.Encoding (encodeUtf8)
import           Data.IORef
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)
import           Foreign.C.String (CString)
import           Foreign.Marshal.Utils (with)
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.HashMap.Strict as M
import qualified GHC.Exts as Exts

import qualified Mapnik.Bindings.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/attribute.hpp>"
C.include "<mapnik/params.hpp>"
C.include "<mapnik/datasource.hpp>"
C.include "<mapnik/featureset.hpp>"
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



unsafeNewParameters :: (Ptr (Ptr Parameters) -> IO ()) -> IO Parameters
unsafeNewParameters = mkUnsafeNew Parameters destroyParameters

getParameters :: Datasource -> IO Parameters
getParameters ds = unsafeNewParameters $ \ ptr ->
  [C.block|void{
  *$(parameters** ptr) = new parameters((*$fptr-ptr:(datasource_ptr *ds))->params());
  }|]

instance Exts.IsList Parameters where
  type Item Parameters = Parameter
  fromList = paramsFromMap . Exts.fromList
  toList = Exts.toList . paramsToMap

paramsFromMap :: Mapnik.Parameters -> Parameters
paramsFromMap ps = unsafePerformIO $ do
  p <- emptyValues
  forM_ (M.toList ps) $ \(encodeUtf8 -> k, val) -> withV val $ \v ->
    [C.block|void {
      std::string k($bs-ptr:k, $bs-len:k);
      (*$fptr-ptr:(parameters *p))[k] = *$(value_holder *v);
    }|]
  return p

emptyValues :: IO Parameters
emptyValues = fmap Parameters . newForeignPtr destroyParameters =<< [C.exp|parameters *{ new parameters }|]

paramsToMap :: Parameters -> Mapnik.Parameters
paramsToMap p = unsafePerformIO $ do
  ref <- newIORef []
  let cb :: CString -> C.CInt -> Ptr Param -> IO ()
      cb k l v = do
        key <- packCStringLen (k, fromIntegral l)
        val <- peekV v
        modifyIORef' ref ((key,val):)
  [C.safeBlock|void {
     for (parameters::const_iterator it = $fptr-ptr:(parameters *p)->begin();
          it != $fptr-ptr:(parameters *p)->end();
          ++it)
     {
       $fun:(void (*cb)(char*, int, value_holder*))(
          const_cast<char*>(it->first.c_str()),
          it->first.size(),
          const_cast<value_holder*>(&it->second)
          );
     }
  }|]
  M.fromList <$> (decodeUtf8Keys "paramsToMap" =<< readIORef ref)


type FeatureSet = (Vector Text, [Feature])

features :: Datasource -> Query -> IO FeatureSet
features ds query = withQuery query $ \q -> do
  feats <- newIORef []
  fields <- newIORef []
  let cbFeat :: Ptr FeaturePtr -> IO ()
      cbFeat p = do
        f <- Feature.unCreate p
        modifyIORef' feats (f:)
      cbField :: CString -> C.CInt -> C.CSize -> IO ()
      cbField p len i = do
        f <- packCStringLen (p, fromIntegral len)
        modifyIORef' fields ((f,i):)
  [C.catchBlock|do {
    auto ds = $fptr-ptr:(datasource_ptr *ds)->get();
    if (!ds) break;
    auto fs = ds->features(*$(query *q));
    if ( !is_valid(fs) ) break;
    feature_ptr last;
    while (auto feat = fs->next() ) {
      $fun:(void (*cbFeat)(feature_ptr*))(&feat);
      last = feat;
    }
    if (last && last->context()) {
      auto ctx = last->context();
      for (auto it=ctx->begin(); it!=ctx->end(); ++it) {
        $fun:(void (*cbField)(char *, int, size_t))(
          const_cast<char*>(it->first.c_str()),
          it->first.size(),
          it->second
        );
      }
    }
  } while(0);|]
  fs <- V.fromList . map fst . sortBy (compare `on` snd)
    <$> (decodeUtf8Keys "features" =<< readIORef fields)
  (,) <$> pure fs
      <*> (reverse  <$> readIORef feats)
  where

featuresAtPoint :: Datasource -> Pair -> Double -> IO FeatureSet
featuresAtPoint aTODO = undefined

data Query = Query
  { _queryBox              :: !Box
  , _queryUnBufferedBox    :: !Box
  , _queryResolution       :: !Pair
  , _queryScaleDenominator :: !Double
  , _queryFilterFactor     :: !Double
  , _queryPropertyNames    :: ![Text] --XXX a set really
  , _queryVariables        :: !Attributes
  }
  deriving (Eq, Show)

queryBox :: Box -> Query
queryBox b = Query
  { _queryBox = b
  , _queryUnBufferedBox = b
  , _queryResolution=Pair 1 1
  , _queryScaleDenominator = 1
  , _queryFilterFactor = 1
  , _queryPropertyNames=[]
  , _queryVariables = M.empty
  }


data HsDatasource where
  HsVector ::
    { _extent             :: !Box
    , fieldNames         :: !(Vector Text)
    , getFeatures        :: !(Query -> IO [Feature])
    , getFeaturesAtPoint :: !(Pair -> Double -> IO [Feature])
    } -> HsDatasource
  HsRaster :: RasterType a =>
    { _extent             :: !Box
    , fieldNames         :: !(Vector Text)
    , getRasters         :: !(Query -> IO [Raster a])
    , getFeaturesAtPoint :: !(Pair -> Double -> IO [Feature])
    } -> HsDatasource



createHsDatasource :: HsDatasource -> IO Datasource
createHsDatasource HsVector{..} = with _extent $ \e -> unsafeNew $ \ ptr -> do
  fs <- $(C.mkFunPtr [t|Ptr FeatureCtx -> Ptr FeatureList -> Ptr QueryPtr -> IO (Ptr ())|]) getFeatures'
  fsp <- $(C.mkFunPtr [t|Ptr FeatureCtx -> Ptr FeatureList -> C.CDouble -> C.CDouble -> C.CDouble -> IO (Ptr ())|]) getFeaturesAtPoint'
  [C.block|void {
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
    [C.block|void {
      auto ds = dynamic_cast<hs_datasource*>((*$(datasource_ptr **ptr))->get());
      ds->push_key(std::string($bs-ptr:v, $bs-len:v));
    }|]

  where
    getFeatures' ctx fs q = catchingExceptions $
      mapM_ (pushBack ctx fs) =<< getFeatures =<< unCreateQuery q

    getFeaturesAtPoint' ctx fs (realToFrac -> x) (realToFrac -> y) (realToFrac -> tol) =
      catchingExceptions $
        mapM_ (pushBack ctx fs) =<< getFeaturesAtPoint (Pair x y) tol

createHsDatasource HsRaster{..} = with _extent $ \e -> unsafeNew $ \ ptr -> do
  fs <- $(C.mkFunPtr [t|Ptr FeatureCtx -> Ptr FeatureList -> Ptr QueryPtr -> IO (Ptr ())|]) getFeatures'
  fsp <- $(C.mkFunPtr [t|Ptr FeatureCtx -> Ptr FeatureList -> C.CDouble -> C.CDouble -> C.CDouble -> IO (Ptr ())|]) getFeaturesAtPoint'
  [C.block|void {
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
    [C.block|void {
      auto ds = dynamic_cast<hs_datasource*>((*$(datasource_ptr **ptr))->get());
      ds->push_key(std::string($bs-ptr:v, $bs-len:v));
    }|]

  where
    getFeatures' ctx fs q = catchingExceptions $ do
      rs <- getRasters =<< unCreateQuery q
      forM_ rs $ \r -> do
        f <- Feature.create ctx $ feature {raster=Just (SomeRaster r)}
        [C.block|void {
          $(feature_list *fs)->push_back(*$fptr-ptr:(feature_ptr *f)); }
        |]

    getFeaturesAtPoint' ctx fs (realToFrac -> x) (realToFrac -> y) (realToFrac -> tol) =
      catchingExceptions $
        mapM_ (pushBack ctx fs) =<< getFeaturesAtPoint (Pair x y) tol

pushBack :: Ptr FeatureCtx -> Ptr FeatureList -> Feature -> IO ()
pushBack ctx fs = \f -> do
  f' <- Feature.create ctx f
  [C.block|void {
    $(feature_list *fs)->push_back(*$fptr-ptr:(feature_ptr *f')); }
  |]


withQuery :: Query -> (Ptr QueryPtr -> IO a) -> IO a
withQuery query f =
  with _queryBox $ \pBox ->
  with _queryUnBufferedBox $ \uBox ->
  withAttributes _queryVariables $ \attrs ->
    let alloc = C.withPtr_ $ \p -> [C.block|void {
      auto q = new query( *$(bbox *pBox)
                        , std::tuple<double,double>( $(double resx)
                                                   , $(double resy))
                        , $(double scale)
                        , *$(bbox *uBox)
                        );
      q->set_filter_factor($(double ff));
      q->set_variables(*$(attributes *attrs));
      *$(query **p) = q;
      }|]
    in bracket alloc dealloc enter

  where
    dealloc p = [C.exp|void { delete $(query *p) }|]
    enter p = do
      forM_ _queryPropertyNames $ \(encodeUtf8 -> pname) ->
        [C.block|void {
          $(query *p)->add_property_name(std::string($bs-ptr:pname, $bs-len:pname));
        }|]
      f p
    Query { _queryResolution = Pair (realToFrac -> resx) (realToFrac -> resy)
          , _queryScaleDenominator = (realToFrac -> scale)
          , _queryFilterFactor = (realToFrac -> ff)
          , ..
          } = query


unCreateQuery :: Ptr QueryPtr -> IO Query
unCreateQuery q = do
  (   realToFrac -> resx
    , realToFrac -> resy
    , realToFrac -> _queryScaleDenominator
    , realToFrac -> _queryFilterFactor
    , _queryBox
    , _queryUnBufferedBox
    , varPtr
    ) <- C.withPtrs_ $ \(x,y,s,f,b,ub,vs) ->
    [C.block|void{
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
  ref <- newIORef  []
  let cb :: CString -> C.CInt -> IO ()
      cb s l = packCStringLen (s,fromIntegral l) >>= \s' -> modifyIORef' ref (s':)
  [C.safeBlock|void {
    auto const names = $(query *q)->property_names();
    for (auto it=names.begin(); it!=names.end(); ++it) {
      $fun:(void (*cb)(char *, int))(const_cast<char *>(it->c_str()), it->size());
    }
  }|]
  _queryPropertyNames <- mapM (decodeUtf8Ctx "unCreateQuery") =<< readIORef ref
  _queryVariables <- extractAttributes varPtr
  return Query{_queryResolution=Pair resx resy, ..}

withAttributes :: Attributes -> (Ptr Attributes -> IO a) -> IO a
withAttributes attrs f = bracket alloc dealloc enter where
  alloc = [C.exp|attributes * { new attributes } |]
  enter p = do
    forM_ (M.toList attrs) $ \(encodeUtf8 -> k, val) -> withV val $ \v ->
      [C.block|void {
        (*$(attributes *p))[std::string($bs-ptr:k, $bs-ptr:k)] = *$(value *v);
      }|]
    f p
  dealloc p = [C.exp|void { delete $(attributes *p) }|]


extractAttributes :: Ptr Attributes -> IO Attributes
extractAttributes p = do
  ref <- newIORef []
  let cb :: CString -> C.CInt -> Ptr Value -> IO ()
      cb k l v = do
        key <- packCStringLen (k, fromIntegral l)
        val <- peekV v
        modifyIORef' ref ((key,val):)
  [C.safeBlock|void {
     for (attributes::const_iterator it = $(attributes *p)->begin();
          it != $(attributes *p)->end();
          ++it)
     {
       $fun:(void (*cb)(char*, int, value*))(
          const_cast<char*>(it->first.c_str()),
          it->first.size(),
          const_cast<value*>(&it->second)
          );
     }
  }|]
  fmap M.fromList . decodeUtf8Keys "extractAttributes" =<< readIORef ref

makeFields ''Query

queryBoxProps :: Box -> [Text] -> Query
queryBoxProps b ps = queryBox b & propertyNames .~ ps

