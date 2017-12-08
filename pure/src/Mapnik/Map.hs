{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Mapnik.Map where

import Mapnik.Imports
import Mapnik.Common
import Mapnik.Color
import Mapnik.Enums
import Mapnik.Parameter
import Data.Monoid (mempty)
import Data.Default
import Mapnik.Style
import Mapnik.Layer

import Prelude hiding (map)


data Map = Map
  { backgroundColor        :: !(Maybe Color)
  , backgroundImage        :: !(Maybe FilePath)
  , backgroundImageCompOp  :: !(Maybe CompositeMode)
  , backgroundImageOpacity :: !(Maybe Double)
  , srs                    :: !(Maybe Proj4)
  , bufferSize             :: !(Maybe Int)
  , maximumExtent          :: !(Maybe Box)
  , fontDirectory          :: !(Maybe FilePath)
  , basePath               :: !(Maybe FilePath)
  , fontSets               :: !FontSetMap
  , styles                 :: !Styles
  , layers                 :: ![Layer]
  , parameters             :: !Parameters
  } deriving (Eq, Show, Generic)
deriveMapnikJSON ''Map

instance Default Map where
  def = Map
    { backgroundColor        = Nothing
    , backgroundImage        = Nothing
    , backgroundImageCompOp  = Nothing
    , backgroundImageOpacity = Nothing
    , srs                    = Nothing
    , bufferSize             = Nothing
    , maximumExtent          = Nothing
    , fontDirectory          = Nothing
    , basePath               = Nothing
    , fontSets               = mempty
    , styles                 = mempty
    , layers                 = mempty
    , parameters             = mempty
    }
