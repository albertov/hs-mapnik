{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Mapnik.Common where

import Mapnik.Imports
import Data.Aeson
import Data.Word
import GHC.Generics
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Generic as G
import Foreign.Storable
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr
import Foreign.C.Types (CDouble)

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

data Dash = Dash {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Eq, Show, Generic)

instance ToJSON Dash where toJSON (Dash a b) = toJSON [a,b]
instance FromJSON Dash where
  parseJSON = withArray "Dash" $ \a ->
    if G.length a == 2
       then Dash <$> parseJSON (a G.! 0)  <*> parseJSON (a G.! 1)
       else fail "dash should have 2 elems"

instance Storable Dash where
  sizeOf   _ = 2 * sizeOf (undefined :: CDouble)
  alignment _ = alignment (undefined :: CDouble)
  peek p = Dash <$> (realToFrac <$> peek @CDouble (castPtr p))
                <*> (realToFrac <$> peek @CDouble (castPtr p `advancePtr` 1))
  poke p (Dash (realToFrac -> a) (realToFrac -> b)) = do
    poke @CDouble (castPtr p) a
    poke @CDouble (castPtr p `advancePtr` 1) b

type DashArray = Vector Dash

type Opacity = Double
type Proj4 = Text
