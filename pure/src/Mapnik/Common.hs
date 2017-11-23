{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mapnik.Common where

import Mapnik.Imports
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Vector.Storable (Vector)
import qualified Data.HashMap.Strict as M
import Foreign.Storable
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr
import Foreign.C.Types (CDouble)

data Box = Box { minx, miny, maxx, maxy :: {-# UNPACK #-}!Double }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Transform = Transform Text
  deriving (Generic)
  deriving newtype (Eq, Show, ToJSON, FromJSON, IsString)

newtype Expression = Expression Text
  deriving (Generic)
  deriving newtype (Eq, Show, ToJSON, FromJSON, IsString)

newtype PathExpression = PathExpression Text
  deriving (Generic)
  deriving newtype (Eq, Show, ToJSON, FromJSON, IsString)


newtype FontFeatureSettings = FontFeatureSettings Text
  deriving (Generic)
  deriving newtype (Eq, Show, ToJSON, FromJSON, IsString)

data Dash = Dash {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show, Generic)
deriveMapnikJSON ''Dash

type FaceName = Text
type FontSetName = Text
type FontSetMap = M.HashMap FontSetName FontSet
type FontSet = [FaceName]

instance Storable Dash where
  sizeOf   _ = 2 * sizeOf (undefined :: CDouble)
  alignment _ = alignment (undefined :: CDouble)
  peek p = Dash <$> (realToFrac <$> peek @CDouble (castPtr p))
                <*> (realToFrac <$> peek @CDouble (castPtr p `advancePtr` 1))
  poke p (Dash (realToFrac -> a) (realToFrac -> b)) = do
    poke @CDouble (castPtr p) a
    poke @CDouble (castPtr p `advancePtr` 1) b

type DashArray = Vector Dash
type Proj4 = Text
