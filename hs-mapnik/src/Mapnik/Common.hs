{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Mapnik.Common where

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
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance IsString Color where
  fromString = ColorName . fromString


type Opacity = Double
type Proj4 = Text
