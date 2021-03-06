{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
import           Mapnik.Bindings.Datasource (paramsFromMap)
import           Mapnik.Bindings.Style as Style
import           Mapnik.Bindings.Symbolizer as Symbolizer
import           Mapnik.Bindings.Rule as Rule
import           Mapnik.Bindings.Expression as Expression

import           Control.Exception
import           Control.Monad
import           Data.HashMap.Strict (toList)
import qualified GHC.Exts as Exts
import           Prelude hiding (filter)
import           System.FilePath ((</>))

class ToMapnik a where
  type MapnikType a :: *
  toMapnik :: a -> IO (MapnikType a)


instance (MapnikType s ~ Datasource, ToMapnik s) => ToMapnik (Mapnik.Map s) where
  {-# SPECIALIZE instance ToMapnik (Mapnik.Map Mapnik.Datasource) #-}
  {-# SPECIALIZE instance ToMapnik (Mapnik.Map Datasource.HsDatasource) #-}
  {-# SPECIALIZE instance ToMapnik (Mapnik.Map (Either Datasource.HsDatasource Mapnik.Datasource)) #-}
  type MapnikType (Mapnik.Map s) = Map
  toMapnik Mapnik.Map{..} = do
    m' <- Map.create
    forM_ backgroundColor        (setBackground m')
    forM_ backgroundImage        (setBackgroundImage m')
    forM_ backgroundImageCompOp  (setBackgroundImageCompOp m')
    forM_ backgroundImageOpacity (setBackgroundImageOpacity m')
    forM_ srs                    (Map.setSrs m')
    forM_ bufferSize             (Map.setBufferSize m')
    forM_ maximumExtent          (Map.setMaxExtent m')
    forM_ fontDirectory          (setAndRegisterFontDirectory basePath m')
    forM_ basePath               (Map.setBasePath m')
    forM_ layers                 (addLayer m' <=< toMapnik)
    forM_ (toList styles)        (\(k,v) -> insertStyle m' k
                                  =<< toMapnikStyle fontSets v)
    forM_ (toList fontSets)      (uncurry (insertFontSet m'))
    setExtraParameters m'        (paramsFromMap parameters)
    return m'

    where
    toMapnikStyle fontMap Mapnik.Style {..} = do
      s <- Style.create
      forM_ opacity             (Style.setOpacity s)
      forM_ imageFiltersInflate (Style.setImageFiltersInflate s)
      forM_ rules               (addRule s <=< toMapnikRule fontMap)
      Style.setFilters s filters
      Style.setDirectFilters s directFilters
      forM_ compOp              (Style.setCompOp s)
      forM_ filterMode          (Style.setFilterMode s)
      return s

    toMapnikRule fontMap Mapnik.Rule {..} = do
      r <- Rule.create
      forM_ name                    (Rule.setName r)
      forM_ filter                  (Rule.setFilter r <=< toMapnik)
      forM_ hasElse                 (Rule.setElse r)
      forM_ hasAlso                 (Rule.setAlso r)
      forM_ minimumScaleDenominator (Rule.setMinScale r)
      forM_ maximumScaleDenominator (Rule.setMaxScale r)
      forM_ symbolizers             (appendSymbolizer r <=< Symbolizer.create fontMap)
      return r

    setAndRegisterFontDirectory mBase m d = do
      Map.setFontDirectory m d
      ok <- Map.registerFonts m (ensureRelative mBase d)
      unless ok (throwIO (ConfigError ("Could not register fonts at " ++ show d)))

    ensureRelative Nothing s = s
    ensureRelative (Just b) s = b </> s

instance (MapnikType s ~ Datasource, ToMapnik s) => ToMapnik (Mapnik.Layer s) where
  type MapnikType (Mapnik.Layer s) = Layer
  {-# SPECIALIZE instance ToMapnik (Mapnik.Layer Mapnik.Datasource) #-}
  {-# SPECIALIZE instance ToMapnik (Mapnik.Layer Datasource.HsDatasource) #-}
  {-# SPECIALIZE instance ToMapnik (Mapnik.Layer (Either Datasource.HsDatasource Mapnik.Datasource)) #-}
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

instance ToMapnik Datasource.HsDatasource where
  type MapnikType Datasource.HsDatasource = Datasource
  toMapnik = Datasource.createHsDatasource

instance ToMapnik Mapnik.Datasource where
  type MapnikType Mapnik.Datasource = Datasource
  toMapnik (Mapnik.Datasource ps) = Datasource.create =<< toMapnik ps

instance (ToMapnik a, ToMapnik b, MapnikType a ~ MapnikType b) => ToMapnik (Either a b) where
  {-# SPECIALIZE instance ToMapnik (Either Datasource.HsDatasource Mapnik.Datasource) #-}
  type MapnikType (Either a b) = MapnikType a
  toMapnik = either toMapnik toMapnik


instance ToMapnik Mapnik.Parameters where
  type MapnikType Mapnik.Parameters = Parameters
  toMapnik = return . Exts.fromList . Exts.toList

instance ToMapnik Mapnik.Expression where
  type MapnikType Mapnik.Expression = Expression
  toMapnik = either (throwIO . userError . ("Invalid expression: " ++)) return
           . Expression.parse . (\(Mapnik.Expression e) -> e)
