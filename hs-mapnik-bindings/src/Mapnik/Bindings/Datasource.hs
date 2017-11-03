{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Bindings.Datasource (
  Datasource
, Parameters
, ParamValue
, ToParam (..)
, (.=)
, unsafeNew
, create
, fromList
) where

import           Mapnik.Bindings
import           Control.Monad ((<=<), forM_)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)
import           System.IO.Unsafe (unsafePerformIO)
import qualified GHC.Exts as Exts
import           Data.String(IsString(..))

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/params.hpp>"
C.include "<mapnik/datasource.hpp>"
C.include "<mapnik/datasource_cache.hpp>"

C.using "namespace mapnik"

--
-- * Layer


foreign import ccall "&hs_mapnik_destroy_Parameters" destroyParameters :: FinalizerPtr Parameters
foreign import ccall "&hs_mapnik_destroy_Datasource" destroyDatasource :: FinalizerPtr Datasource

unsafeNew :: (Ptr (Ptr Datasource) -> IO ()) -> IO Datasource
unsafeNew = fmap Datasource . newForeignPtr destroyDatasource <=< C.withPtr_

create :: Parameters -> IO Datasource
create params = unsafeNew $ \ ptr ->
  [C.catchBlock|
  datasource_ptr p = datasource_cache::instance().create(*$fptr-ptr:(parameters *params));
  *$(datasource_ptr** ptr) = new datasource_ptr(p);
  |]

data ParamValue = StringParam Text
                | DoubleParam Double
                | IntParam Int
                | BoolParam Bool
  deriving (Show, Eq)

class ToParam p where
  toParam :: p -> ParamValue
instance ToParam ParamValue where toParam = id
instance ToParam String where toParam = StringParam . fromString
instance ToParam Text where toParam = StringParam
instance ToParam Double where toParam = DoubleParam
instance ToParam Int where toParam = IntParam
instance ToParam Bool where toParam = BoolParam

type Parameter = (Text, ParamValue)

(.=) :: ToParam v => String -> v -> Parameter
k .= v = (fromString k, toParam v)

instance IsString ParamValue where
  fromString = StringParam . fromString

instance Exts.IsList Parameters where
  type Item Parameters = Parameter
  fromList = fromList
  toList = error "toList is not implemented for Parameters"

fromList :: [(Text,ParamValue)] -> Parameters
fromList ps = unsafePerformIO $ do
  p <- emptyParams
  forM_ ps $ \(encodeUtf8 -> k, value) ->
    case value of
      StringParam (encodeUtf8 -> v) ->
        [C.block|void {
          std::string k($bs-ptr:k, $bs-len:k);
          std::string v($bs-ptr:v, $bs-len:v);
          (*$fptr-ptr:(parameters *p))[k] = value_holder(v);
        }|]
      DoubleParam (realToFrac -> v) ->
        [C.block|void {
          std::string k($bs-ptr:k, $bs-len:k);
          (*$fptr-ptr:(parameters *p))[k] = value_holder($(double v));
        }|]
      IntParam (fromIntegral -> v) ->
        [C.block|void {
          std::string k($bs-ptr:k, $bs-len:k);
          (*$fptr-ptr:(parameters *p))[k] = value_holder($(int v));
        }|]
      BoolParam (fromIntegral . fromEnum -> v) ->
        [C.block|void {
          std::string k($bs-ptr:k, $bs-len:k);
          (*$fptr-ptr:(parameters *p))[k] = value_holder($(int v)?true:false);
        }|]
  return p

emptyParams :: IO Parameters
emptyParams = fmap Parameters . newForeignPtr destroyParameters =<< [C.exp|parameters *{ new parameters }|]
