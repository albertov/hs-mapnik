{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Mapnik.Bindings.VectorTile.Types (
  MVTile(..)
, XYZ (..)
) where

import Data.ByteString (ByteString)
import Data.Word
import GHC.Generics (Generic)

-- | A pbf encoded Mapbox Vector Tile
newtype MVTile = MVTile {pbfData :: ByteString}
  deriving (Generic)
  deriving newtype (Eq, Show)

data XYZ = XYZ !Word64 !Word64 !Word64
  deriving (Eq, Show, Generic)

