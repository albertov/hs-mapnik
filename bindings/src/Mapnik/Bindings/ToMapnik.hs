{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.ToMapnik (ToMapnik(..)) where

import qualified Mapnik
import           Mapnik.Lens
import           Mapnik.Bindings
import           Mapnik.Bindings.Layer as Layer
import           Mapnik.Bindings.Map as Map
import qualified Mapnik.Bindings.Datasource as Datasource
import           Mapnik.Bindings.Style as Style
import           Mapnik.Bindings.Symbolizer as Symbolizer
import           Mapnik.Bindings.Rule as Rule
import           Mapnik.Bindings.Expression as Expression

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.HashMap.Strict (toList)
import qualified GHC.Exts as Exts
import           Prelude hiding (filter)

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
  toMapnik m = do
    m' <- Map.create 400 400
    forM_ (m^.backgroundColor)        (setBackground m' <=< toMapnik)
    forM_ (m^.backgroundImage)        (setBackgroundImage m')
    forM_ (m^.backgroundImageCompOp)  (setBackgroundImageCompOp m')
    forM_ (m^.backgroundImageOpacity) (setBackgroundImageOpacity m')
    forM_ (m^.srs)                    (Map.setSrs m')
    forM_ (m^.bufferSize)             (Map.setBufferSize m')
    forM_ (m^.maximumExtent)          (Map.setMaxExtent m')
    forM_ (m^.fontDirectory)          (Map.setFontDirectory m')
    forM_ (m^.layers)                 (addLayer m' <=< toMapnik)
    forM_ (m^.styleLst)               (\(k,v) -> insertStyle m' k =<< toMapnik v)
    return m'

instance ToMapnik Mapnik.Style where
  type MapnikType Mapnik.Style = Style
  toMapnik Mapnik.Style {..} = do
    s <- Style.create
    forM_ opacity             (Style.setOpacity s)
    forM_ imageFiltersInflate (Style.setImageFiltersInflate s)
    forM_ rules               (addRule s <=< toMapnik)
    return s

instance ToMapnik Mapnik.Rule where
  type MapnikType Mapnik.Rule = Rule
  toMapnik Mapnik.Rule {..} = do
    r <- Rule.create
    forM_ name                    (Rule.setName r)
    forM_ filter                  (Rule.setFilter r <=< toMapnik)
    forM_ minimumScaleDenominator (Rule.setMinScale r)
    forM_ maximumScaleDenominator (Rule.setMaxScale r)
    forM_ symbolizers             (appendSymbolizer r <=< toMapnik)
    return r

instance ToMapnik Mapnik.Symbolizer where
  type MapnikType Mapnik.Symbolizer = Symbolizer
  toMapnik = Symbolizer.create

instance ToMapnik Mapnik.Layer where
  type MapnikType Mapnik.Layer = Layer
  toMapnik Mapnik.Layer {..} = do
    l <- Layer.create name
    forM_ dataSource              (Layer.setDatasource l <=< toMapnik)
    forM_ srs                     (Layer.setSrs l)
    forM_ minimumScaleDenominator (setMinScaleDenominator l)
    forM_ maximumScaleDenominator (setMaxScaleDenominator l)
    forM_ queryable               (setQueryable l)
    forM_ clearLabelCache         (setClearLabelCache l)
    forM_ cacheFeatures           (setCacheFeatures l)
    forM_ groupBy                 (setGroupBy l)
    forM_ bufferSize              (Layer.setBufferSize l)
    forM_ maximumExtent           (Layer.setMaxExtent l)
    forM_ styles                  (addStyle l)
    return l

instance ToMapnik Mapnik.Datasource where
  type MapnikType Mapnik.Datasource = Datasource
  toMapnik (Mapnik.Datasource ps) = Datasource.create =<< toMapnik ps

instance ToMapnik Mapnik.Parameters where
  type MapnikType Mapnik.Parameters = Parameters
  toMapnik = return . Exts.fromList . Exts.toList

instance ToMapnik Mapnik.Expression where
  type MapnikType Mapnik.Expression = Expression
  toMapnik = either (throwIO . userError . ("Invalid expression: " ++)) return
           . Expression.parse . (\(Mapnik.Expression e) -> e)
