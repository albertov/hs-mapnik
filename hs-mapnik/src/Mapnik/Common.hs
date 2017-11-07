{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Mapnik.Common where

import Mapnik.Imports
import Data.Aeson
import Data.Word
import GHC.Generics
import Data.String (IsString(..))
import Data.Text (Text)

data Box = Box { minx, miny, maxx, maxy :: {-# UNPACK #-}!Double }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Color
  = RGBA { red, green, blue, alpha :: {-# UNPACK #-} !Word8 }
  | ColorName Text
  deriving (Eq, Show)

deriveMapnikJSON 0 ''Color

instance IsString Color where
  fromString = ColorName . fromString

newtype Transform = Transform {unTransform :: Text}
  deriving (Generic)
  deriving newtype (Show, ToJSON, FromJSON, Eq, IsString)

newtype Expression = Expression {unExpression :: Text}
  deriving (Generic)
  deriving newtype (Show, ToJSON, FromJSON, Eq, IsString)

type Opacity = Double
type Proj4 = Text
