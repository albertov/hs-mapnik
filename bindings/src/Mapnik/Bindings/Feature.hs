{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.Feature (
  Feature (..)
, Geometry (..)
, createFeature
) where

import           Mapnik.Bindings
import           Mapnik.Bindings.Util

import           Data.ByteString (ByteString)
import           Data.String (IsString(..))
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/feature.hpp>"
C.include "<mapnik/feature_factory.hpp>"

C.using "namespace mapnik"

data Geometry
  = GeometryWKB !ByteString
  | GeometryWKT !ByteString
  deriving (Eq, Show)

instance IsString Geometry where
  fromString = GeometryWKT . fromString

data Feature = Feature
  { fid      :: !Int
  , geometry :: !Geometry
  }
  deriving (Eq, Show)

foreign import ccall "&hs_mapnik_destroy_Feature" destroyFeature :: FinalizerPtr FeaturePtr

unsafeNew :: (Ptr (Ptr FeaturePtr) -> IO ()) -> IO FeaturePtr
unsafeNew = mkUnsafeNew FeaturePtr destroyFeature

createFeature :: Ptr FeatureCtx -> Feature -> IO FeaturePtr
createFeature ctx Feature{ fid = (fromIntegral -> fid')
                         , ..
                         } = unsafeNew $ \ret ->
  [CU.block|void {
  *$(feature_ptr **ret) = new feature_ptr(
    feature_factory::create(*$(context_ptr *ctx), $(value_integer fid'))
    );
  }|]
