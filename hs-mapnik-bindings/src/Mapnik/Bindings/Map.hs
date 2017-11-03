{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.Map (
  Map
, Box (..)
, AspectFixMode (..)
, unsafeNew
, create
, loadXml
, loadXmlFile
, zoom
, zoomAll
, zoomToBox
, setBasePath
, setBackgroundImage
, setFontDirectory
, setSrs
, setBufferSize
, setAspectFixMode
, resize
, render
, addLayer
, removeAllLayers
) where

import           Mapnik.Bindings
import qualified Mapnik.Bindings.Image as Image
import           Control.Monad ((<=<))
import           Data.String (fromString)
import           Data.ByteString (ByteString)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/agg_renderer.hpp>"
C.include "<mapnik/map.hpp>"
C.include "<mapnik/layer.hpp>"
C.include "<mapnik/load_map.hpp>"

C.using "namespace mapnik"

--
-- * Map

data AspectFixMode = GrowBox
                   | GrowCanvas
                   | ShrinkBox
                   | ShrinkCanvas
                   | AdjustBoxWidth
                   | AdjustBoxHeight
                   | AdjustCanvasWidth
                   | AdjustCanvasHeight
                   | Respect
                   deriving (Eq, Show)

foreign import ccall "&hs_mapnik_destroy_Map" destroyMap :: FinalizerPtr Map

unsafeNew :: (Ptr (Ptr Map) -> IO ()) -> IO Map
unsafeNew = fmap Map . newForeignPtr destroyMap <=< C.withPtr_

create :: Int -> Int -> IO Map
create (fromIntegral -> width) (fromIntegral -> height) =
  unsafeNew $ \p -> [C.catchBlock|*$(Map** p) = new Map($(int width), $(int height));|]

loadXmlFile :: Map -> FilePath -> IO ()
loadXmlFile m (fromString -> path) =
  [C.catchBlock|
  mapnik::load_map(*$fptr-ptr:(Map *m), std::string($bs-ptr:path, $bs-len:path));
  |]

loadXml :: Map -> ByteString -> IO ()
loadXml m str =
  [C.catchBlock|
  mapnik::load_map_string(*$fptr-ptr:(Map *m), std::string($bs-ptr:str, $bs-len:str));
  |]

setBackgroundImage :: Map -> FilePath -> IO ()
setBackgroundImage m (fromString -> path) =
  [C.block| void {
  $fptr-ptr:(Map *m)->set_background_image(std::string($bs-ptr:path, $bs-len:path));
  }|]

setBasePath :: Map -> FilePath -> IO ()
setBasePath m (fromString -> path) =
  [C.block| void {
  $fptr-ptr:(Map *m)->set_base_path(std::string($bs-ptr:path, $bs-len:path));
  }|]

setFontDirectory :: Map -> FilePath -> IO ()
setFontDirectory m (fromString -> path) =
  [C.block| void {
  $fptr-ptr:(Map *m)->set_font_directory(std::string($bs-ptr:path, $bs-len:path));
  }|]
  
setSrs :: Map -> String -> IO ()
setSrs m (fromString -> srs) =
  [C.block|void { $fptr-ptr:(Map *m)->set_srs(std::string($bs-ptr:srs, $bs-len:srs));}|]

setAspectFixMode :: Map -> AspectFixMode -> IO ()
setAspectFixMode m GrowBox = [C.block|void { $fptr-ptr:(Map *m)->set_aspect_fix_mode(Map::GROW_BBOX);}|]
setAspectFixMode m GrowCanvas = [C.block|void { $fptr-ptr:(Map *m)->set_aspect_fix_mode(Map::GROW_CANVAS);}|]
setAspectFixMode m ShrinkBox = [C.block|void { $fptr-ptr:(Map *m)->set_aspect_fix_mode(Map::SHRINK_BBOX);}|]
setAspectFixMode m ShrinkCanvas = [C.block|void { $fptr-ptr:(Map *m)->set_aspect_fix_mode(Map::SHRINK_CANVAS);}|]
setAspectFixMode m AdjustBoxWidth = [C.block|void { $fptr-ptr:(Map *m)->set_aspect_fix_mode(Map::ADJUST_BBOX_WIDTH);}|]
setAspectFixMode m AdjustBoxHeight = [C.block|void { $fptr-ptr:(Map *m)->set_aspect_fix_mode(Map::ADJUST_BBOX_HEIGHT);}|]
setAspectFixMode m AdjustCanvasWidth = [C.block|void { $fptr-ptr:(Map *m)->set_aspect_fix_mode(Map::ADJUST_CANVAS_WIDTH);}|]
setAspectFixMode m AdjustCanvasHeight = [C.block|void { $fptr-ptr:(Map *m)->set_aspect_fix_mode(Map::ADJUST_CANVAS_HEIGHT);}|]
setAspectFixMode m Respect = [C.block|void { $fptr-ptr:(Map *m)->set_aspect_fix_mode(Map::RESPECT);}|]

setBufferSize :: Map -> Int -> IO ()
setBufferSize m (fromIntegral -> size) =
  [C.block|void {$fptr-ptr:(Map *m)->set_buffer_size($(int size));}|]

resize :: Map -> Int -> Int -> IO ()
resize m (fromIntegral -> width) (fromIntegral -> height) =
  [C.block|void {$fptr-ptr:(Map *m)->resize($(int width), $(int height));}|]

zoom :: Map -> Double -> IO ()
zoom m (realToFrac -> z) =
  [C.catchBlock|$fptr-ptr:(Map *m)->zoom($(double z));|]

zoomAll :: Map -> IO ()
zoomAll m = [C.catchBlock|$fptr-ptr:(Map *m)->zoom_all();|]

zoomToBox :: Map -> Box -> IO ()
zoomToBox m (Box (realToFrac -> x0) (realToFrac -> y0) (realToFrac -> x1) (realToFrac -> y1)) = 
  [C.catchBlock|
  $fptr-ptr:(Map *m)->zoom_to_box(mapnik::box2d<double>($(double x0), $(double y0), $(double x1), $(double y1)));
  |]

render :: Map -> Double -> IO Image.Image
render m (realToFrac -> scale) = Image.unsafeNew $ \ptr ->
  [C.catchBlock|
  mapnik::Map *m = $fptr-ptr:(Map *m);
  mapnik::image_rgba8 *im = new mapnik::image_rgba8(m->width(), m->height());
  try {
    mapnik::agg_renderer<mapnik::image_rgba8> ren(*m, *im, $(double scale));
    ren.apply();
    *$(image_rgba8** ptr) = im;
  } catch (...) {
    delete im;
    throw;
  }
  |]

addLayer :: Map -> Layer -> IO ()
addLayer m l = [C.block|void {
  $fptr-ptr:(Map *m)->add_layer(*$fptr-ptr:(layer *l));
  }|]

removeAllLayers :: Map -> IO ()
removeAllLayers m =
  [C.block| void { $fptr-ptr:(Map *m)->layers().clear(); }|]

