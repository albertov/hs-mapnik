{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.ToMapnik (ToMapnik(..)) where

import qualified Mapnik
import           Mapnik.Bindings
import           Mapnik.Bindings.Layer as Layer
import           Mapnik.Bindings.Map as Map
import           Mapnik.Bindings.Datasource as Datasource
import           Mapnik.Bindings.Color as Color
import           Mapnik.Bindings.Style as Style
import           Mapnik.Bindings.Symbolizer as Symbolizer
import           Mapnik.Bindings.Rule as Rule
import           Mapnik.Bindings.Expression as Expression

import           Control.Exception
import           Control.Monad
import           Data.HashMap.Strict (toList)
import qualified GHC.Exts as Exts

class ToMapnik a where
  type MapnikType a :: *
  toMapnik :: a -> IO (MapnikType a)


#define toMapnikId(X) \
instance ToMapnik X where {\
  type MapnikType X = X;\
  toMapnik = return;\
}

toMapnikId(Map)
toMapnikId(Layer)
toMapnikId(Datasource)
toMapnikId(Parameters)
toMapnikId(Color)
toMapnikId(Style)
toMapnikId(Rule)
toMapnikId(Symbolizer)
toMapnikId(Expression)

instance ToMapnik Mapnik.Map where
  type MapnikType Mapnik.Map = Map
  toMapnik Mapnik.Map {..} = do
    m <- Map.create 400 400
    forM_ _mapBackgroundColor        (setBackground m <=< toMapnik)
    forM_ _mapBackgroundImage        (setBackgroundImage m)
  --forM_ _mapBackgroundImageCompOp  (setBackgroundImageCompOp m <=< toMapnik)
    forM_ _mapBackgroundImageOpacity (setBackgroundImageOpacity m)
    forM_ _mapSrs                    (Map.setSrs m)
    forM_ _mapBufferSize             (Map.setBufferSize m)
    forM_ _mapMaximumExtent          (Map.setMaxExtent m)
    forM_ _mapFontDirectory          (Map.setFontDirectory m)
    forM_ _mapLayers                 (addLayer m <=< toMapnik)
    forM_ (toList _mapStyles)        (\(k,v) -> insertStyle m k =<< toMapnik v)
    return m

instance ToMapnik Mapnik.Style where
  type MapnikType Mapnik.Style = Style
  toMapnik Mapnik.Style {..} = do
    s <- Style.create
    forM_ _styleOpacity             (Style.setOpacity s)
    forM_ _styleImageFiltersInflate (Style.setImageFiltersInflate s)
    forM_ _styleRules               (addRule s <=< toMapnik)
    return s

instance ToMapnik Mapnik.Rule where
  type MapnikType Mapnik.Rule = Rule
  toMapnik Mapnik.Rule {..} = do
    r <- Rule.create
    forM_ _ruleName                    (Rule.setName r)
    forM_ _ruleFilter                  (Rule.setFilter r <=< toMapnik)
    forM_ _ruleMinimumScaleDenominator (Rule.setMinScale r)
    forM_ _ruleMaximumScaleDenominator (Rule.setMaxScale r)
    forM_ _ruleSymbolizers             (appendSymbolizer r <=< toMapnik)
    return r

instance ToMapnik Mapnik.Symbolizer where
  type MapnikType Mapnik.Symbolizer = Symbolizer
  toMapnik = Symbolizer.create

instance ToMapnik Mapnik.Layer where
  type MapnikType Mapnik.Layer = Layer
  toMapnik Mapnik.Layer {..} = do
    l <- Layer.create _layerName
    setDatasource l =<< toMapnik _layerDataSource
    forM_ _layerSrs                     (Layer.setSrs l)
    forM_ _layerMinimumScaleDenominator (setMinScaleDenominator l)
    forM_ _layerMaximumScaleDenominator (setMaxScaleDenominator l)
    forM_ _layerQueryable               (setQueryable l)
    forM_ _layerClearLabelCache         (setClearLabelCache l)
    forM_ _layerCacheFeatures           (setCacheFeatures l)
    forM_ _layerGroupBy                 (setGroupBy l)
    forM_ _layerBufferSize              (Layer.setBufferSize l)
    forM_ _layerMaximumExtent           (Layer.setMaxExtent l)
    forM_ _layerStyles                  (addStyle l)
    return l

instance ToMapnik Mapnik.Datasource where
  type MapnikType Mapnik.Datasource = Datasource
  toMapnik (Mapnik.Datasource ps) = Datasource.create =<< toMapnik ps

instance ToMapnik Mapnik.Parameters where
  type MapnikType Mapnik.Parameters = Parameters
  toMapnik = return . Exts.fromList . Exts.toList

instance ToMapnik Mapnik.Color where
  type MapnikType Mapnik.Color = Color
  toMapnik = maybe (throwIO (userError "Invalid color")) return 
           . Color.create

instance ToMapnik Mapnik.Expression where
  type MapnikType Mapnik.Expression = Expression
  toMapnik = either (throwIO . userError . ("Invalid expression: " ++)) return
           . Expression.parse . Mapnik.unExpression
