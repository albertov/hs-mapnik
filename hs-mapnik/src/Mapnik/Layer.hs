{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Layer where

import Mapnik.Imports
import Mapnik.Datasource
import Mapnik.Style
import Mapnik.Rule

import Data.Text (Text)

data Layer = Layer
  { _layerName                    :: !Text
  , _layerDataSource              :: !Datasource
  , _layerStyles                  :: ![StyleName]
  , _layerSrs                     :: !(Maybe Proj4)
  , _layerMinimumScaleDenominator :: !(Maybe Double)
  , _layerMaximumScaleDenominator :: !(Maybe Double)
  , _layerQueryable               :: !(Maybe Bool)
  , _layerClearLabelCache         :: !(Maybe Bool)
  , _layerCacheFeatures           :: !(Maybe Bool)
  , _layerGroupBy                 :: !(Maybe Text)
  , _layerBufferSize              :: !(Maybe Int)
  , _layerMaximumExtent           :: Maybe Box
  } deriving (Eq, Show, Generic)
makeClassy ''Layer
makeFields ''Layer

instance ToJSON Layer where toEncoding = mapnikJsonEncoder 6
instance FromJSON Layer where parseJSON = mapnikJsonDecoder 6

mkLayer :: Text -> Datasource -> Layer
mkLayer n ds = Layer
  { _layerName                    = n
  , _layerDataSource              = ds
  , _layerStyles                  = []
  , _layerSrs                     = Nothing
  , _layerMinimumScaleDenominator = Nothing
  , _layerMaximumScaleDenominator = Nothing
  , _layerQueryable               = Nothing
  , _layerClearLabelCache         = Nothing
  , _layerCacheFeatures           = Nothing
  , _layerGroupBy                 = Nothing
  , _layerBufferSize              = Nothing
  , _layerMaximumExtent           = Nothing
  }
