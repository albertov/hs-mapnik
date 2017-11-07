{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
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
, setBackground
, getBackground
, setBackgroundImage
, getBackgroundImage
, setBackgroundImageOpacity
, getBackgroundImageOpacity
, getBackgroundImageCompOp
, setBackgroundImageCompOp
, setFontDirectory
, getFontDirectory
, getSrs
, setSrs
, getBufferSize
, setBufferSize
, setAspectFixMode
, getMaxExtent
, setMaxExtent
, getLayers
, getStyles
, resize
, render
, addLayer
, insertStyle
, removeAllLayers
) where

import           Mapnik (StyleName, AspectFixMode(..), CompositeMode)
import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Color()
import qualified Mapnik.Bindings.Image as Image
import qualified Mapnik.Bindings.Layer as Layer
import qualified Mapnik.Bindings.Style as Style

import           Data.IORef
import           Data.String (fromString)
import           Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.ByteString (ByteString)
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)
import           Foreign.C.String (CString)
import           Foreign.Storable (poke)
import           Foreign.Marshal.Utils (with)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/agg_renderer.hpp>"
C.include "<mapnik/feature_type_style.hpp>"
C.include "<mapnik/map.hpp>"
C.include "<mapnik/layer.hpp>"
C.include "<mapnik/load_map.hpp>"

C.using "namespace mapnik"

--
-- * Map


foreign import ccall "&hs_mapnik_destroy_Map" destroyMap :: FinalizerPtr Map

unsafeNew :: (Ptr (Ptr Map) -> IO ()) -> IO Map
unsafeNew = mkUnsafeNew Map destroyMap

create :: Int -> Int -> IO Map
create (fromIntegral -> width) (fromIntegral -> height) =
  unsafeNew $ \p -> [C.block|void{*$(Map** p) = new Map($(int width), $(int height));}|]

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

getBackground :: Map -> IO (Maybe Color)
getBackground m = newMaybe $ \(has,p) ->
  [C.block|void {
  auto mColor = $fptr-ptr:(Map *m)->background();
  if (mColor) {
    *$(int *has) = 1;
    *$(color *p) = *mColor;
  } else {
    *$(int *has) = 0;
  }
  }|]

setBackground :: Map -> Color -> IO ()
setBackground m col = with col $ \colPtr ->
  [C.block|void {
  $fptr-ptr:(Map *m)->set_background(*$(color *colPtr));
  }|]

setBackgroundImage :: Map -> FilePath -> IO ()
setBackgroundImage m (fromString -> path) =
  [C.block| void {
  $fptr-ptr:(Map *m)->set_background_image(std::string($bs-ptr:path, $bs-len:path));
  }|]

getBackgroundImage :: Map -> IO (Maybe FilePath)
getBackgroundImage m = fmap (fmap unpack) $ newTextMaybe $ \(p,len) ->
  [C.block| void {
  auto mPath = $fptr-ptr:(Map *m)->background_image();
  if (mPath) {
    *$(int *len) = mPath->size();
    *$(char **p) = strdup(mPath->c_str());
  } else {
    *$(char **p) = NULL;
  }
  }|]


setBackgroundImageCompOp :: Map -> CompositeMode -> IO ()
setBackgroundImageCompOp m (fromIntegral . fromEnum -> v) =
  [C.block| void {
  $fptr-ptr:(Map *m)->set_background_image_comp_op(static_cast<composite_mode_e>($(int v)));
  }|]

getBackgroundImageCompOp :: Map -> IO (Maybe CompositeMode)
getBackgroundImageCompOp m =
  fmap (fmap (toEnum . fromIntegral) ) $ newMaybe $ \(has, p) ->
    [C.block| void {
    composite_mode_e mode = $fptr-ptr:(Map *m)->background_image_comp_op();
    Map def;
    if (mode != def.background_image_comp_op()) {
      *$(int *has) = 1;
      *$(int *p) = static_cast<int>(mode);
    } else {
      *$(int *has) = 0;
    }
    }|]

setBackgroundImageOpacity :: Map -> Double -> IO ()
setBackgroundImageOpacity m (realToFrac -> opacity) =
  [C.block| void {
  $fptr-ptr:(Map *m)->set_background_image_opacity($(double opacity));
  }|]

getBackgroundImageOpacity :: Map -> IO Double
getBackgroundImageOpacity m = realToFrac <$>
  [C.exp| float { $fptr-ptr:(Map *m)->background_image_opacity() }|]

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

getFontDirectory :: Map -> IO (Maybe FilePath)
getFontDirectory m = fmap (fmap unpack) $ newTextMaybe $ \(p,len) ->
  [C.block| void {
  auto mPath = $fptr-ptr:(Map *m)->font_directory();
  if (mPath) {
    *$(int *len) = mPath->size();
    *$(char **p) = strdup(mPath->c_str());
  } else {
    *$(char **p) = NULL;
  }
  }|]
  
setSrs :: Map -> Text -> IO ()
setSrs m (encodeUtf8 -> srs) =
  [C.block|void { $fptr-ptr:(Map *m)->set_srs(std::string($bs-ptr:srs, $bs-len:srs));}|]

getSrs :: Map -> IO Text
getSrs m = newText $ \(p,len) -> [C.block|void {
  std::string const &srs = $fptr-ptr:(Map *m)->srs();
  *$(int *len) = srs.size();
  *$(char **p) = strdup (srs.c_str());
  }|]

setAspectFixMode :: Map -> AspectFixMode -> IO ()
setAspectFixMode m (fromIntegral . fromEnum  -> v) =
  [C.block|void { $fptr-ptr:(Map *m)->set_aspect_fix_mode(static_cast<Map::aspect_fix_mode>($(int v)));}|]

setBufferSize :: Map -> Int -> IO ()
setBufferSize m (fromIntegral -> size) =
  [C.block|void {$fptr-ptr:(Map *m)->set_buffer_size($(int size));}|]

getBufferSize :: Map -> IO Int
getBufferSize m = fromIntegral <$> [C.exp|int { $fptr-ptr:(Map *m)->buffer_size() }|]

resize :: Map -> Int -> Int -> IO ()
resize m (fromIntegral -> width) (fromIntegral -> height) =
  [C.block|void {$fptr-ptr:(Map *m)->resize($(int width), $(int height));}|]

zoom :: Map -> Double -> IO ()
zoom m (realToFrac -> z) =
  [C.block|void {$fptr-ptr:(Map *m)->zoom($(double z));}|]

zoomAll :: Map -> IO ()
zoomAll m = [C.catchBlock|$fptr-ptr:(Map *m)->zoom_all();|]

zoomToBox :: Map -> Box -> IO ()
zoomToBox m (Box (realToFrac -> x0) (realToFrac -> y0) (realToFrac -> x1) (realToFrac -> y1)) = 
  [C.block|void {
  $fptr-ptr:(Map *m)->zoom_to_box(mapnik::box2d<double>($(double x0), $(double y0), $(double x1), $(double y1)));
  }|]

render :: Map -> Double -> IO Image
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

getLayers :: Map -> IO [Layer]
getLayers m = do
  layersRef <- newIORef []
  let callback :: Ptr Layer -> IO ()
      callback ptr = do
        layer <- Layer.unsafeNew (flip poke ptr)
        modifyIORef' layersRef (layer:)
  [C.block|void {
  typedef std::vector<layer> layer_list;
  layer_list const& layers = $fptr-ptr:(Map *m)->layers();
  for (layer_list::const_iterator it=layers.begin(); it!=layers.end(); ++it) {
    $fun:(void (*callback)(layer*))(new layer(*it));
  }
  }|]
  reverse <$> readIORef layersRef

getStyles :: Map -> IO [(StyleName,Style)]
getStyles m = do
  stylesRef <- newIORef []
  let callback :: CString -> C.CInt -> Ptr Style -> IO ()
      callback ptr (fromIntegral -> len) ptrStyle = do
        style <- Style.unsafeNew (flip poke ptrStyle)
        styleName <- decodeUtf8 <$> unsafePackMallocCStringLen (ptr, len)
        modifyIORef' stylesRef ((styleName,style):)
  [C.block|void {
  typedef std::map<std::string,feature_type_style> style_map;
  style_map const& styles = $fptr-ptr:(Map *m)->styles();
  for (style_map::const_iterator it=styles.begin(); it!=styles.end(); ++it) {
    $fun:(void (*callback)(char*, int, feature_type_style*))(strdup(it->first.c_str()), it->first.size(), new feature_type_style(it->second));
  }
  }|]
  reverse <$> readIORef stylesRef


insertStyle :: Map -> StyleName -> Style -> IO ()
insertStyle m (encodeUtf8 -> n) l = [C.block|void {
  $fptr-ptr:(Map *m)->insert_style(
      std::string($bs-ptr:n, $bs-len:n), *$fptr-ptr:(feature_type_style *l)
      );
  }|]

removeAllLayers :: Map -> IO ()
removeAllLayers m =
  [C.block| void { $fptr-ptr:(Map *m)->layers().clear(); }|]

getMaxExtent :: Map -> IO (Maybe Box)
getMaxExtent m =  do
  (   has
    , realToFrac -> minx
    , realToFrac -> miny
    , realToFrac -> maxx
    , realToFrac -> maxy
    ) <- C.withPtrs_ $ \(has,x0,y0,x1,y1) ->
    [C.block|void {
      auto res = $fptr-ptr:(Map *m)->maximum_extent();
      if (res) {
        *$(double *x0) = res->minx();
        *$(double *y0) = res->miny();
        *$(double *x1) = res->maxx();
        *$(double *y1) = res->maxy();
        *$(int *has) = 1;
      } else {
        *$(int *has) = 0;
      }
      }|]
  return $ if has==1 then Just Box{..} else Nothing

setMaxExtent :: Map -> Box -> IO ()
setMaxExtent m (Box (realToFrac -> x0) (realToFrac -> y0) (realToFrac -> x1) (realToFrac -> y1)) = 
  [C.block|void {
  $fptr-ptr:(Map *m)->set_maximum_extent(mapnik::box2d<double>($(double x0), $(double y0), $(double x1), $(double y1)));
  }|]
