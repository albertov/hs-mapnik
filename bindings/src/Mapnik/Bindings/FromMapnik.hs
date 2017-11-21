{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Mapnik.Bindings.FromMapnik where

import qualified Mapnik
import qualified Mapnik.Map as Mapnik
import qualified Mapnik.Layer as Mapnik
import qualified Mapnik.Style as Mapnik
import qualified Mapnik.Rule as Mapnik
import qualified Mapnik.Common as Mapnik
import           Mapnik.Bindings.Types
import qualified Mapnik.Bindings.Map as Map
import qualified Mapnik.Bindings.Layer as Layer
import qualified Mapnik.Bindings.Style as Style
import qualified Mapnik.Bindings.Rule as Rule
import qualified Mapnik.Bindings.Expression as Expression
import qualified Mapnik.Bindings.Datasource as Datasource
import qualified Mapnik.Bindings.Symbolizer as Symbolizer

import qualified Data.HashMap.Strict as M
import           Prelude hiding (filter)

class FromMapnik a where
  type HsType a :: *
  fromMapnik :: a -> IO (HsType a)

instance FromMapnik a => FromMapnik (Maybe a) where
  type HsType (Maybe a) = Maybe (HsType a)
  fromMapnik Nothing = return Nothing
  fromMapnik (Just a) = Just <$> fromMapnik a

instance FromMapnik Map where
  type HsType Map = Mapnik.Map
  fromMapnik m = do
    backgroundColor <- Map.getBackground m
    backgroundImage <- Map.getBackgroundImage m
    backgroundImageCompOp <- Map.getBackgroundImageCompOp m
    backgroundImageOpacity <- Just <$> Map.getBackgroundImageOpacity m
    srs <- Just <$> Map.getSrs m
    bufferSize <- Just <$> Map.getBufferSize m
    maximumExtent <- Map.getMaxExtent m
    fontDirectory <- Map.getFontDirectory m
    layers <- mapM fromMapnik =<< Map.getLayers m
    styles <- M.fromList <$> (mapM (\(k,v) -> (k,) <$> fromMapnik v) =<< Map.getStyles m)
    fontSets <- Map.getFontSetMap m
    return Mapnik.Map{..}

instance FromMapnik Expression where
  type HsType Expression = Mapnik.Expression
  fromMapnik = return . Mapnik.Expression . Expression.toText

instance FromMapnik Rule where
  type HsType Rule = Mapnik.Rule
  fromMapnik r = do
    name                    <- Just <$> Rule.getName r
    filter                  <- fromMapnik =<< Rule.getFilter r
    minimumScaleDenominator <- Just <$> Rule.getMinScale r
    maximumScaleDenominator <- Just <$> Rule.getMaxScale r
    symbolizers             <- mapM fromMapnik =<< Rule.getSymbolizers r
    return Mapnik.Rule {..}


instance FromMapnik Layer where
  type HsType Layer = Mapnik.Layer
  fromMapnik l = do
    name                    <- Layer.getName l
    dataSource              <- fromMapnik =<< Layer.getDatasource l
    srs                     <- Just <$> Layer.getSrs l
    minimumScaleDenominator <- Just <$> Layer.getMinScaleDenominator l
    maximumScaleDenominator <- Just <$> Layer.getMaxScaleDenominator l
    queryable               <- Just <$> Layer.getQueryable l
    clearLabelCache         <- Just <$> Layer.getClearLabelCache l
    cacheFeatures           <- Just <$> Layer.getCacheFeatures l
    groupBy                 <- Just <$> Layer.getGroupBy l
    bufferSize              <- Layer.getBufferSize l
    maximumExtent           <- Layer.getMaxExtent l
    styles                  <- Layer.getStyles l
    return Mapnik.Layer{..}

instance FromMapnik Style where
  type HsType Style = Mapnik.Style
  fromMapnik s = do
    opacity             <- Just <$> Style.getOpacity s
    imageFiltersInflate <- Just <$> Style.getImageFiltersInflate s
    rules               <- mapM fromMapnik =<< Style.getRules s
    return Mapnik.Style{..}

instance FromMapnik Datasource where
  type HsType Datasource = Mapnik.Datasource
  fromMapnik = fmap (Mapnik.Datasource . M.fromList . Datasource.paramsToList)
             . Datasource.getParameters

instance FromMapnik Symbolizer where
  type HsType Symbolizer = Mapnik.Symbolizer
  fromMapnik = Symbolizer.unCreate

