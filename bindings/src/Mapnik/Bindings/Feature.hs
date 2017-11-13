module Mapnik.Bindings.Feature (
  Feature (..)
, Geometry (..)
, createFeature
) where

import           Mapnik.Bindings
import           Data.ByteString (ByteString)
import           Data.String (IsString(..))
import           Data.Text (Text)

data Geometry
  = GeometryWKB !ByteString
  | GeometryWKT !Text
  deriving (Eq, Show)

instance IsString Geometry where
  fromString = GeometryWKT . fromString

data Feature = Feature
  { fid      :: !Int
  , geometry :: !Geometry
  }
  deriving (Eq, Show)

createFeature :: Feature -> IO FeaturePtr
createFeature = undefined
