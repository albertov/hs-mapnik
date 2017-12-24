{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Mapnik.Layer where

import Mapnik.Imports
import Mapnik.Common
import Mapnik.Datasource
import Mapnik.Style

import Data.Text (Text)

data Layer s = Layer
  { name                    :: !Text
  , dataSource              :: !(Maybe s)
  , styles                  :: ![StyleName]
  , srs                     :: !(Maybe Proj4)
  , minimumScaleDenominator :: !(Maybe Double)
  , maximumScaleDenominator :: !(Maybe Double)
  , queryable               :: !(Maybe Bool)
  , clearLabelCache         :: !(Maybe Bool)
  , cacheFeatures           :: !(Maybe Bool)
  , groupBy                 :: !(Maybe Text)
  , bufferSize              :: !(Maybe Int)
  , maximumExtent           :: Maybe Box
  } deriving (Eq, Show, Generic)
deriveMapnikJSON ''Layer

layer :: Text -> Layer s
layer n = Layer
  { name                    = n
  , dataSource              = Nothing
  , styles                  = []
  , srs                     = Nothing
  , minimumScaleDenominator = Nothing
  , maximumScaleDenominator = Nothing
  , queryable               = Nothing
  , clearLabelCache         = Nothing
  , cacheFeatures           = Nothing
  , groupBy                 = Nothing
  , bufferSize              = Nothing
  , maximumExtent           = Nothing
  }
