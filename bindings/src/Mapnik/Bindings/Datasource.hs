{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import           Control.Exception (throwIO)
import           Control.Monad (forM_)
import           Data.ByteString (ByteString)
import           Data.ByteString.Unsafe (unsafePackMallocCString)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.IORef
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.Storable
import           Foreign.Marshal.Array (advancePtr)
import           Foreign.C.Types (CDouble)
import           Foreign.C.String (CString)
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

data Query = Query
  { bbox :: !Box
  }
  deriving (Eq, Show)

data Geometry
  = GeometryWKB !ByteString
  | GeometryWKT !Text
  deriving (Eq, Show)

data Feature = Feature
  { geometry :: !Geometry
  }
  deriving (Eq, Show)

data Point = Point { x, y :: !Double }
  deriving (Eq, Show)

instance Storable Point where
  sizeOf   _ = 2 * sizeOf (undefined :: CDouble)
  alignment _ = alignment (undefined :: CDouble)
  peek p = Point <$> (realToFrac <$> peek @CDouble (castPtr p))
                 <*> (realToFrac <$> peek @CDouble (castPtr p `advancePtr` 1))
  poke p (Point (realToFrac -> a) (realToFrac -> b)) = do
    poke @CDouble (castPtr p) a
    poke @CDouble (castPtr p `advancePtr` 1) b

data HsDatasource = HsDatasource
  { name               :: !Text
  , extent             :: !Box
  , getFeatures        :: Query -> IO [Feature]
  , getFeaturesAtPoint :: Point -> Double -> IO [Feature]
  }


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

createHsDatasource :: HsDatasource -> IO Datasource
createHsDatasource _ = unsafeNew $ \ ptr -> do
  features <- $(C.mkFunPtr [t|Ptr FeatureList -> Ptr QueryPtr -> IO ()|]) $ \fs q -> do
    putStrLn "Hi there!"
    return ()
  featuresAtPoint <- $(C.mkFunPtr [t|Ptr FeatureList -> C.CDouble -> C.CDouble -> C.CDouble -> IO ()|]) $ \fs x y tol -> do
    return ()
  [CU.block|void {
  datasource_ptr p = std::make_shared<hs_datasource>(
    "hs_layer",
    datasource::Vector, 
    datasource_geometry_t::Collection,
    box2d<double>(0,0,100,100),
    $(features_callback features),
    $(features_at_point_callback featuresAtPoint)
    );
  *$(datasource_ptr** ptr) = new datasource_ptr(p);
  }|]

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
