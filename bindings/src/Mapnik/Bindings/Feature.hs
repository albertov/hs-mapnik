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

import           Control.Monad (forM_)
import           Data.ByteString (ByteString)
import           Data.String (IsString(..))
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
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

data Field
  = TextField   !Text
  | BoolField   !Bool
  | IntField    !Int
  | DoubleField !Double
  | NullField
  deriving (Eq, Show)

instance IsString Field where
  fromString = TextField . fromString

data Geometry
  = GeometryWKB !ByteString
  | GeometryWKT !ByteString
  deriving (Eq, Show)

instance IsString Geometry where
  fromString = GeometryWKT . fromString

data Feature = Feature
  { fid      :: !Int
  , geometry :: !Geometry
  , fields   :: ![Field]
  }
  deriving (Eq, Show)

foreign import ccall "&hs_mapnik_destroy_Feature" destroyFeature :: FinalizerPtr FeaturePtr

unsafeNew :: (Ptr (Ptr FeaturePtr) -> IO ()) -> IO FeaturePtr
unsafeNew = mkUnsafeNew FeaturePtr destroyFeature

createFeature :: [Text] -> Ptr FeatureCtx -> Feature -> IO FeaturePtr
createFeature names ctx f = unsafeNew $ \ret -> do
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
  forM_ (zip names fields) $ \(encodeUtf8 -> k, val) ->
    case val of
      TextField (encodeUtf8 -> v) ->
        [CU.block|void{
          auto feat = **$(feature_ptr **ret);
          auto key = std::string($bs-ptr:k, $bs-len:k);

          static const mapnik::transcoder tr("utf-8");
          feat->put_new(key, tr.transcode($bs-ptr:v, $bs-len:v));
        }|]
  where 
    Feature { fid = (fromIntegral -> fid')
            , ..
            }  = f
