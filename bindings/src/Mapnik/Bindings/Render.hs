{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.Render (
  RenderSettings(..)
, render
, renderSettings
) where

import           Mapnik (Proj4, AspectFixMode(..))
import           Mapnik.Bindings
import           Mapnik.Bindings.Variant
import           Mapnik.Bindings.Orphans()
import qualified Mapnik.Bindings.Image as Image
import qualified Mapnik.Bindings.Map as Map

import           Control.Exception (bracket)
import           Control.Monad (forM_)
import           Data.Monoid (mempty)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.HashMap.Strict as M
import           Foreign.Marshal.Utils (with)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline.Cpp.Exceptions as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/attribute.hpp>"
C.include "<mapnik/agg_renderer.hpp>"
C.include "<mapnik/map.hpp>"

C.using "namespace mapnik"
C.verbatim "typedef box2d<double> bbox;"


data RenderSettings = RenderSettings
  { width         :: !Int
  , height        :: !Int
  , extent        :: !Box
  , variables     :: !Attributes
  , scaleFactor   :: !Double
  , srs           :: !(Maybe Proj4)
  , aspectFixMode :: !AspectFixMode
  } deriving (Eq, Show)

renderSettings :: Int -> Int -> Box -> RenderSettings
renderSettings w h e = RenderSettings w h e mempty 1 Nothing Respect

render :: Map -> RenderSettings-> IO Image
render m cfg = Image.unsafeNew $ \ptr ->
               withAttributes $ \vars ->
               with extent $ \box -> do
  forM_ srs (Map.setSrs m)
  Map.setAspectFixMode m aspectFixMode
  forM_ (M.toList variables) $ \(encodeUtf8 -> k, val) -> withV val $ \v ->
    [CU.block|void {
      std::string k($bs-ptr:k, $bs-len:k);
      attributes &m = *$(attributes *vars);
      m[k] = *$(value *v);
    }|]
  [C.catchBlock|
  mapnik::Map &m = *$fptr-ptr:(Map *m);
  m.resize($(int w), $(int h)); //FIXME: The query resolution comes out wrong if we dont' do this
  m.zoom_to_box(*$(bbox *box)); //FIXME: the extent we pass in the request seems to be ignored
  mapnik::request req($(int w), $(int h), *$(bbox *box));
  mapnik::image_rgba8 *im = new mapnik::image_rgba8($(int w), $(int h));
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
    RenderSettings
      { width = (fromIntegral -> w)
      , height = (fromIntegral -> h)
      , scaleFactor = (realToFrac -> scale)
      , ..
      } = cfg
    withAttributes = bracket alloc dealloc where
      alloc = [CU.exp|attributes * { new attributes }|]
      dealloc p = [CU.exp|void { delete $(attributes *p) }|]
