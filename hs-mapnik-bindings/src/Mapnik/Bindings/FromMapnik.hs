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
    _mapSrs <- Just <$> getSrs m
    _mapBufferSize <- Just <$> getBufferSize m
    _mapMaximumExtent <- undefined
    _mapFontDirectory <- getFontDirectory m
    _mapLayers <- mapM fromMapnik =<< getLayers m
    _mapStyles <- HM.fromList
             <$> (mapM (\(k,v) -> (k,) <$> fromMapnik v) =<< getStyles m)
    return Mapnik.Map{..}

instance FromMapnik Color where
  type HsType Color = Mapnik.Color
  fromMapnik = undefined

instance FromMapnik Layer where
  type HsType Layer = Mapnik.Layer
  fromMapnik = undefined

instance FromMapnik Style where
  type HsType Style = Mapnik.Style
  fromMapnik = undefined
