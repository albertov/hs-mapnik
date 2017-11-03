{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Map where

import Mapnik.Imports
import Data.Monoid (mempty)
import Mapnik.Style 
import Mapnik.Layer

import Prelude hiding (map)


data Map = Map
  { _mapBackgroundColor        :: Maybe Color
  , _mapBackgroundImage        :: Maybe FilePath
  , _mapBackgroundImageCompOp  :: Maybe CompOp
  , _mapBackgroundImageOpacity :: Maybe Opacity
  , _mapSrs                    :: Maybe Proj4
  , _mapBufferSize             :: Maybe Int
  , _mapMaximumExtent          :: Maybe Box
  , _mapFontDirectory          :: Maybe FilePath
  , _mapStyles                 :: Styles
  , _mapLayers                 :: [Layer]
  } deriving (Eq, Show, Generic)

instance ToJSON Map where toEncoding = mapnikJsonEncoder 4
instance FromJSON Map where parseJSON = mapnikJsonDecoder 4


empty :: Map
empty = Map
  { _mapBackgroundColor        = Nothing
  , _mapBackgroundImage        = Nothing
  , _mapBackgroundImageCompOp  = Nothing
  , _mapBackgroundImageOpacity = Nothing
  , _mapSrs                    = Nothing
  , _mapBufferSize             = Nothing
  , _mapMaximumExtent          = Nothing
  , _mapFontDirectory          = Nothing
  , _mapStyles                 = mempty
  , _mapLayers                 = mempty
  }

makeClassy ''Map
makeFields ''Map
