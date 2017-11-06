{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Mapnik.Bindings.FromMapnik where

import qualified Mapnik
import           Mapnik.Bindings
import qualified Mapnik.Bindings.Color as Color
import qualified Mapnik.Bindings.Map as Map
import qualified Mapnik.Bindings.Layer as Layer
import qualified Mapnik.Bindings.Style as Style
import qualified Mapnik.Bindings.Rule as Rule
import qualified Mapnik.Bindings.Expression as Expression
import qualified Mapnik.Bindings.Datasource as Datasource
import qualified Mapnik.Bindings.Symbolizer as Symbolizer
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
    _mapBackgroundColor <- fromMapnik =<< Map.getBackground m
    _mapBackgroundImage <- Map.getBackgroundImage m
    _mapBackgroundImageCompOp <- undefined
    _mapBackgroundImageOpacity <- Just <$> Map.getBackgroundImageOpacity m
    _mapSrs <- Just <$> Map.getSrs m
    _mapBufferSize <- Just <$> Map.getBufferSize m
    _mapMaximumExtent <- Map.getMaxExtent m
    _mapFontDirectory <- Map.getFontDirectory m
    _mapLayers <- mapM fromMapnik =<< Map.getLayers m
    _mapStyles <- HM.fromList
             <$> (mapM (\(k,v) -> (k,) <$> fromMapnik v) =<< Map.getStyles m)
    return Mapnik.Map{..}

instance FromMapnik Color where
  type HsType Color = Mapnik.Color
  fromMapnik = Color.unCreate

instance FromMapnik Expression where
  type HsType Expression = Mapnik.Expression
  fromMapnik = return . Mapnik.Expression . Expression.toText

instance FromMapnik Rule where
  type HsType Rule = Mapnik.Rule
  fromMapnik r = do
    _ruleName                    <- Just <$> Rule.getName r
    _ruleFilter                  <- fromMapnik =<< Rule.getFilter r
    _ruleMinimumScaleDenominator <- Just <$> Rule.getMinScale r
    _ruleMaximumScaleDenominator <- Just <$> Rule.getMaxScale r
    _ruleSymbolizers             <- mapM fromMapnik =<< Rule.getSymbolizers r
    return Mapnik.Rule {..}


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
    _styleOpacity             <- Just <$> Style.getOpacity s
    _styleImageFiltersInflate <- Just <$> Style.getImageFiltersInflate s
    _styleRules               <- mapM fromMapnik =<< Style.getRules s
    return Mapnik.Style{..}

instance FromMapnik Datasource where
  type HsType Datasource = Mapnik.Datasource
  fromMapnik = fmap (Mapnik.Datasource . HM.fromList . Datasource.toList)
             . Datasource.getParameters

instance FromMapnik Symbolizer where
  type HsType Symbolizer = Mapnik.Symbolizer
  fromMapnik = Symbolizer.unCreate

