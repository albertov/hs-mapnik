{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.ToMapnik (ToMapnik(..)) where

import qualified Mapnik
import qualified Mapnik.Map as Mapnik
import qualified Mapnik.Layer as Mapnik
import qualified Mapnik.Style as Mapnik
import qualified Mapnik.Rule as Mapnik
import qualified Mapnik.Common as Mapnik
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Layer as Layer
import           Mapnik.Bindings.Map as Map
import qualified Mapnik.Bindings.Datasource as Datasource
import           Mapnik.Bindings.Style as Style
import           Mapnik.Bindings.Symbolizer as Symbolizer
import           Mapnik.Bindings.Rule as Rule
import           Mapnik.Bindings.Expression as Expression

import           Control.Exception
import           Control.Monad
import           Data.HashMap.Strict (toList)
import qualified GHC.Exts as Exts
import           Prelude hiding (filter)

class ToMapnik a where
  type MapnikType a :: *
  toMapnik :: a -> IO (MapnikType a)


instance ToMapnik Mapnik.Map where
  type MapnikType Mapnik.Map = Map
  toMapnik Mapnik.Map{..} = do
    m' <- Map.create
    forM_ backgroundColor        (setBackground m')
    forM_ backgroundImage        (setBackgroundImage m')
    forM_ backgroundImageCompOp  (setBackgroundImageCompOp m')
    forM_ backgroundImageOpacity (setBackgroundImageOpacity m')
    forM_ srs                    (Map.setSrs m')
    forM_ bufferSize             (Map.setBufferSize m')
    forM_ maximumExtent          (Map.setMaxExtent m')
    forM_ fontDirectory          (setAndRegisterFontDirectory m')
    forM_ layers                 (addLayer m' <=< toMapnik)
    forM_ (toList styles)        (\(k,v) -> insertStyle m' k =<< toMapnikStyle fontSets v)
    forM_ (toList fontSets)      (uncurry (insertFontSet m'))
    return m'

    where
    toMapnikStyle fontMap Mapnik.Style {..} = do
      s <- Style.create
      forM_ opacity             (Style.setOpacity s)
      forM_ imageFiltersInflate (Style.setImageFiltersInflate s)
      forM_ rules               (addRule s <=< toMapnikRule fontMap)
      return s

    toMapnikRule fontMap Mapnik.Rule {..} = do
      r <- Rule.create
      forM_ name                    (Rule.setName r)
      forM_ filter                  (Rule.setFilter r <=< toMapnik)
      forM_ minimumScaleDenominator (Rule.setMinScale r)
      forM_ maximumScaleDenominator (Rule.setMaxScale r)
      forM_ symbolizers             (appendSymbolizer r <=< Symbolizer.create fontMap)
      return r

    setAndRegisterFontDirectory m d = do
      Map.setFontDirectory m d
      ok <- Map.registerFonts m d
      unless ok (throwIO (ConfigError ("Could not register fonts at " ++ show d)))

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
