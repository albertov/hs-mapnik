{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Mapnik.Bindings.FromMapnik where

import qualified Mapnik
import           Mapnik.Bindings
import           Mapnik.Bindings.Map as Map
import           Mapnik.Bindings.Layer as Layer
import qualified Data.HashMap.Strict as HM

class FromMapnik a where
  type HsType a :: *
  fromMapnik :: a -> IO (HsType a)

instance FromMapnik a => FromMapnik (Maybe a) where
  type HsType (Maybe a) = Maybe (HsType a)
  fromMapnik Nothing = return Nothing
  fromMapnik (Just a) = Just <$> fromMapnik a

#define fromMapnikId(X) \
instance FromMapnik X where {\
  type HsType X = X;\
  fromMapnik = return;\
}

fromMapnikId(Mapnik.Map)
fromMapnikId(Mapnik.Layer)
fromMapnikId(Mapnik.Datasource)
fromMapnikId(Mapnik.Parameters)
fromMapnikId(Mapnik.Color)
fromMapnikId(Mapnik.Style)
fromMapnikId(Mapnik.Rule)
fromMapnikId(Mapnik.Symbolizer)
fromMapnikId(Mapnik.Expression)

instance FromMapnik Map where
  type HsType Map = Mapnik.Map
  fromMapnik m = do
    _mapBackgroundColor <- fromMapnik =<< getBackground m
    _mapBackgroundImage <- getBackgroundImage m
    _mapBackgroundImageCompOp <- undefined
    _mapBackgroundImageOpacity <- Just <$> getBackgroundImageOpacity m
    _mapSrs <- Just <$> Map.getSrs m
    _mapBufferSize <- Just <$> Map.getBufferSize m
    _mapMaximumExtent <- Map.getMaxExtent m
    _mapFontDirectory <- getFontDirectory m
    _mapLayers <- mapM fromMapnik =<< getLayers m
    _mapStyles <- HM.fromList
             <$> (mapM (\(k,v) -> (k,) <$> fromMapnik v) =<< Map.getStyles m)
    return Mapnik.Map{..}

instance FromMapnik Datasource where
  type HsType Datasource = Mapnik.Datasource
  fromMapnik = undefined

instance FromMapnik Color where
  type HsType Color = Mapnik.Color
  fromMapnik = undefined

instance FromMapnik Layer where
  type HsType Layer = Mapnik.Layer
  fromMapnik l = do
    _layerName                    <- Layer.getName l
    _layerDataSource              <- fromMapnik =<< Layer.getDatasource l
    _layerSrs                     <- Just <$> Layer.getSrs l
    _layerMinimumScaleDenominator <- Just <$> Layer.getMinScaleDenominator l
    _layerMaximumScaleDenominator <- Just <$> Layer.getMaxScaleDenominator l
    _layerQueryable               <- Just <$> Layer.getQueryable l
    _layerClearLabelCache         <- Just <$> Layer.getClearLabelCache l
    _layerCacheFeatures           <- Just <$> Layer.getCacheFeatures l
    _layerGroupBy                 <- Just <$> Layer.getGroupBy l
    _layerBufferSize              <- Layer.getBufferSize l
    _layerMaximumExtent           <- Layer.getMaxExtent l
    _layerStyles                  <- Layer.getStyles l
    return Mapnik.Layer{..}

instance FromMapnik Style where
  type HsType Style = Mapnik.Style
  fromMapnik s = do
    _styleOpacity             <- undefined
    _styleImageFiltersInflate <- undefined
    _styleRules               <- undefined
    return Mapnik.Style{..}
