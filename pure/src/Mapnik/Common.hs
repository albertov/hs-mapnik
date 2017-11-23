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
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word32)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr, unsafeToForeignPtr)
import qualified Data.HashMap.Strict as M
import Foreign.Storable
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr
import Foreign.ForeignPtr
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

newtype PixelRgba8 = PixelRgba8 Word32
  deriving (Generic)
  deriving newtype (Eq, Show, Num, Storable, ToJSON, FromJSON)

unRgba8 :: PixelRgba8 -> Word32
unRgba8 (PixelRgba8 x) = x

type Size = (Int,Int)

data ImageRgba8 = ImageRgba8
  { size   :: !Size
  , pixels :: !(Vector PixelRgba8)
  } deriving (Eq, Show, Generic)
deriveMapnikJSON ''ImageRgba8

toRgba8ByteString :: Vector PixelRgba8 -> ByteString
toRgba8ByteString vec = PS (castForeignPtr fp) off (len*4)
  where (fp,off,len) = unsafeToForeignPtr vec

fromRgba8ByteString :: ByteString -> Vector PixelRgba8
fromRgba8ByteString (PS fp off len) =
  unsafeFromForeignPtr (castForeignPtr fp) off (len `div` 4)

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
