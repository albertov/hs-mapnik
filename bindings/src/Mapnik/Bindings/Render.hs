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

import           Mapnik (Proj4, AspectFixMode(..))
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
import           Control.Monad (forM_)
import           Control.Lens
import           Data.Monoid (mempty)
import           Data.ByteString (useAsCStringLen)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.HashMap.Strict as M
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr



C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/attribute.hpp>"
C.include "<mapnik/agg_renderer.hpp>"
C.include "<mapnik/map.hpp>"

C.using "namespace mapnik"
C.verbatim "typedef box2d<double> bbox;"


data RenderSettings = RenderSettings
  { _renderSettingsWidth         :: !Int
  , _renderSettingsHeight        :: !Int
  , _renderSettingsExtent        :: !Box
  , _renderSettingsVariables     :: !Attributes
  , _renderSettingsScaleFactor   :: !Double
  , _renderSettingsSrs           :: !(Maybe Proj4)
  , _renderSettingsAspectFixMode :: !AspectFixMode
  } deriving (Eq, Show)
makeFields ''RenderSettings

renderSettings :: Int -> Int -> Box -> RenderSettings
renderSettings w h e = RenderSettings w h e mempty 1 Nothing Respect

-- | It is not safe to call this function on the same 'Map' from different
-- threads. It is the responsability of the caller to implement locking, a
-- reosurce pool...
render :: Map -> RenderSettings-> IO Image
render m cfg = Image.unsafeNew $ \ptr ->
               withAttributes $ \vars ->
               with (cfg^.extent) $ \ext ->
               withMaybeSrs (cfg^.srs) $ \(srsPtr, fromIntegral -> srsLen) -> do
  Map.setAspectFixMode m (cfg^.aspectFixMode)
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
  if ($(int srsLen)) {
    oldSrs = m.srs();
    m.set_srs(std::string($(char *srsPtr), $(int srsLen)));
  }
  try {
    mapnik::agg_renderer<mapnik::image_rgba8> ren(m, req, *$(attributes *vars), *im, $(double scale));
    ren.apply();
    *$(image_rgba8** ptr) = im;
    if ($(int srsLen)) {
      m.set_srs(oldSrs);
    }
  } catch (...) {
    delete im;
    if ($(int srsLen)) {
      m.set_srs(oldSrs);
    }
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

    withMaybeSrs Nothing f = f (nullPtr,0)
    withMaybeSrs (Just (encodeUtf8 -> s)) f = useAsCStringLen s f
