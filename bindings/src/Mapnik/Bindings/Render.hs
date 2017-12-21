{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Mapnik.Bindings.Render where

import           Mapnik (Proj4, AspectFixMode(..), Color(RGBA))
import           Mapnik.Lens
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Variant
import           Mapnik.Bindings.Datasource -- For field class defs
import           Mapnik.Bindings.Raster -- For field class defs
import           Mapnik.Bindings.Orphans()
import qualified Mapnik.Bindings.Cpp as C
import qualified Mapnik.Bindings.Image as Image
import qualified Mapnik.Bindings.Map as Map

import           Control.Exception (bracket)
import           Control.Monad (forM_, forM)
import           Control.Lens
import           Data.Monoid (mempty)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.HashMap.Strict as M
import           Foreign.Marshal.Utils (with)



C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/attribute.hpp>"
C.include "<mapnik/agg_renderer.hpp>"
C.include "<mapnik/map.hpp>"

C.using "namespace mapnik"
C.using "bbox = box2d<double>"

data RenderSettings = RenderSettings
  { _renderSettingsWidth           :: !Int
  , _renderSettingsHeight          :: !Int
  , _renderSettingsExtent          :: !Box
  , _renderSettingsVariables       :: !Attributes
  , _renderSettingsScaleFactor     :: !Double
  , _renderSettingsSrs             :: !(Maybe Proj4)
  , _renderSettingsAspectFixMode   :: !AspectFixMode
  , _renderSettingsBackgroundColor :: !(Maybe Color)
  , _renderSettingsLayers          :: !(Maybe [Text])
  } deriving (Eq, Show)
makeFields ''RenderSettings

renderSettings :: Int -> Int -> Box -> RenderSettings
renderSettings w h e =
  RenderSettings w h e mempty 1 Nothing Respect Nothing Nothing

-- | It is not safe to call this function on the same 'Map' from different
-- threads. It is the responsability of the caller to implement locking, a
-- reosurce pool...
render :: Map -> RenderSettings-> IO Image
render m cfg = Image.unsafeNew $ \ptr ->
               withAttributes $ \vars ->
               with (cfg^.extent) $ \ext ->
               bracket setRenderOverrides id $ \ _ -> do
  forM_ (cfg^.variables.to M.toList) $ \(encodeUtf8 -> k, val) -> withV val $ \v ->
    [C.block|void {
      std::string k($bs-ptr:k, $bs-len:k);
      attributes &m = *$(attributes *vars);
      m[k] = *$(value *v);
    }|]
  [C.catchBlock|
  mapnik::Map &m = *$fptr-ptr:(Map *m);
  m.resize($(int w), $(int h)); //FIXME MAPNIK: The query resolution comes out wrong if we dont' do this
  m.zoom_to_box(*$(bbox *ext)); //FIXME MAPNIK: the extent we pass in the request seems to be ignored
  mapnik::request req($(int w), $(int h), *$(bbox *ext));
  mapnik::image_rgba8 *im = new mapnik::image_rgba8($(int w), $(int h));
  std::string oldSrs;
  try {
    mapnik::agg_renderer<mapnik::image_rgba8> ren(m, req, *$(attributes *vars), *im, $(double scale));
    ren.apply();
    *$(image_rgba8** ptr) = im;
  } catch (...) {
    delete im;
    throw;
  }
  |]
  where
    w = cfg^?!width.to fromIntegral
    h = cfg^?!height.to fromIntegral
    scale = cfg^.scaleFactor.to realToFrac
    withAttributes = bracket alloc dealloc where
      alloc = [C.exp|attributes * { new attributes }|]
      dealloc p = [C.exp|void { delete $(attributes *p) }|]

    setRenderOverrides = do
      rollbackSrs <- overrideWith Map.getSrs Map.setSrs (cfg^.srs)
      rollbackBg <- overrideWith (fmap (fromMaybe (RGBA 0 0 0 0)) . Map.getBackground)
                                 Map.setBackground (cfg^.backgroundColor)
      rollbackLayers <- fromMaybe (return ()) <$> forM (cfg^.layers) (\ ls -> do
        Map.setActiveLayers m (Just ls)
        return (Map.setActiveLayers m Nothing)
        )
      return (sequence_ [rollbackSrs, rollbackBg, rollbackLayers])


    overrideWith :: (Map -> IO a) -> (Map -> a -> IO ()) -> Maybe a -> IO (IO ())
    overrideWith get_ set_ = fmap (fromMaybe (return ())) . mapM (\ x -> do
      old <- get_ m
      set_ m x
      return (set_ m old))

