{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.Map (
  Map
, unsafeNew
, create
, loadXml
, loadXmlFile
, fromXml
, toXml
, fromXmlFile
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
, addLayer
, insertStyle
, insertFontSet
, getFontSetMap
, registerFonts
, removeAllLayers
) where

import           Mapnik (Color(..), StyleName, AspectFixMode(..), CompositeMode
                        , FontSetMap, FontSetName)
import qualified Mapnik
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Orphans()
import qualified Mapnik.Bindings.Layer as Layer
import qualified Mapnik.Bindings.Style as Style
import           Mapnik.Bindings.Symbolizer (withFontSet)

import           Data.IORef
import           Data.String (fromString)
import           Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as M
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)
import           Foreign.C.String (CString)
import           Foreign.Storable (poke)
import           Foreign.Marshal.Utils (with)

import qualified Language.C.Inline.Unsafe as CU
import qualified Mapnik.Bindings.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/rule.hpp>"
C.include "<mapnik/feature_type_style.hpp>"
C.include "<mapnik/map.hpp>"
C.include "<mapnik/layer.hpp>"
C.include "<mapnik/load_map.hpp>"
C.include "<mapnik/save_map.hpp>"
C.include "util.hpp"

C.using "namespace mapnik"
C.using "bbox = box2d<double>"

--
-- * Map


foreign import ccall "&hs_mapnik_destroy_Map" destroyMap :: FinalizerPtr Map

unsafeNew :: (Ptr (Ptr Map) -> IO ()) -> IO Map
unsafeNew = mkUnsafeNew Map destroyMap

create :: IO Map
create = unsafeNew $ \p -> [C.block|void{*$(Map** p) = new Map();}|]

loadXmlFile :: Map -> FilePath -> IO ()
loadXmlFile m (encodeUtf8 . fromString -> path) =
  [C.catchBlock|
  mapnik::load_map(*$fptr-ptr:(Map *m), std::string($bs-ptr:path, $bs-len:path));
  |]

fromXmlFile :: FilePath -> IO Map
fromXmlFile f = do {m <- create; loadXmlFile m f; return m}

loadXml :: Map -> ByteString -> IO ()
loadXml m str =
  [C.catchBlock|
  mapnik::load_map_string(*$fptr-ptr:(Map *m), std::string($bs-ptr:str, $bs-len:str));
  |]

toXml :: Map -> IO ByteString
toXml m = newByteString $ \(p, len) ->
  [C.catchBlock|
  auto s = mapnik::save_map_to_string(*$fptr-ptr:(Map *m));
  mallocedString(s, $(char **p), $(int *len));
  |]

fromXml :: ByteString -> IO Map
fromXml s = do {m <- create; loadXml m s; return m}

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
setBackgroundImage m (encodeUtf8 . fromString -> path) =
  [C.block| void {
  $fptr-ptr:(Map *m)->set_background_image(std::string($bs-ptr:path, $bs-len:path));
  }|]


getBackgroundImage :: Map -> IO (Maybe FilePath)
getBackgroundImage m = fmap (fmap unpack) $ newTextMaybe "Map.getBackgroundImage" $ \(p,len) ->
  [C.block| void {
  auto mPath = $fptr-ptr:(Map *m)->background_image();
  if (mPath) {
    mallocedString(*mPath, $(char **p), $(int *len));
  } else {
    *$(char **p) = nullptr;
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
setBasePath m (encodeUtf8 . fromString -> path) =
  [C.block| void {
  $fptr-ptr:(Map *m)->set_base_path(std::string($bs-ptr:path, $bs-len:path));
  }|]

setFontDirectory :: Map -> FilePath -> IO ()
setFontDirectory m (encodeUtf8 . fromString -> path) =
  [C.block| void {
  $fptr-ptr:(Map *m)->set_font_directory(std::string($bs-ptr:path, $bs-len:path));
  }|]

getFontDirectory :: Map -> IO (Maybe FilePath)
getFontDirectory m = fmap (fmap unpack) $ newTextMaybe "Map.getFontDirectory" $ \(p,len) ->
  [C.block| void {
  auto mPath = $fptr-ptr:(Map *m)->font_directory();
  if (mPath) {
    mallocedString(*mPath, $(char **p), $(int *len));
  } else {
    *$(char **p) = nullptr;
  }
  }|]
  
setSrs :: Map -> Text -> IO ()
setSrs m (encodeUtf8 -> srs) =
  [C.block|void { $fptr-ptr:(Map *m)->set_srs(std::string($bs-ptr:srs, $bs-len:srs));}|]

getSrs :: Map -> IO Text
getSrs m = newText "Map.getSrs" $ \(p,len) -> [C.block|void {
  std::string const &srs = $fptr-ptr:(Map *m)->srs();
  mallocedString(srs, $(char **p), $(int *len));
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
zoomToBox m box = with box $ \boxPtr -> 
  [C.block|void {$fptr-ptr:(Map *m)->zoom_to_box(*$(bbox *boxPtr));}|]

addLayer :: Map -> Layer -> IO ()
addLayer m l = [C.block|void {
  $fptr-ptr:(Map *m)->add_layer(*$fptr-ptr:(layer *l));
  }|]

getLayers :: Map -> IO [Layer]
getLayers m = do
  layersRef <- newIORef []
  let callback :: Ptr Layer -> IO ()
      callback ptr = do
        layer <- Layer.unsafeNew (`poke` ptr)
        modifyIORef' layersRef (layer:)
  [C.safeBlock|void {
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
        style <- Style.unsafeNew (`poke` ptrStyle)
        styleName <- unsafePackMallocCStringLen (ptr, len)
        modifyIORef' stylesRef ((styleName,style):)
  [C.safeBlock|void {
  typedef std::map<std::string,feature_type_style> style_map;
  style_map const& styles = $fptr-ptr:(Map *m)->styles();
  for (style_map::const_iterator it=styles.begin(); it!=styles.end(); ++it) {
    char *buf;
    int len;
    mallocedString(it->first, &buf, &len);
    $fun:(void (*callback)(char*, int, feature_type_style*))(buf, len, new feature_type_style(it->second));
  }
  }|]
  reverse <$> (decodeUtf8Keys "Map.getStyles" =<< readIORef stylesRef)


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
getMaxExtent m =  newMaybe $ \(has, p) ->
  [C.block|void {
    auto res = $fptr-ptr:(Map *m)->maximum_extent();
    if (res) {
      *$(bbox *p)  = *res;
      *$(int *has) = 1;
    } else {
      *$(int *has) = 0;
    }
    }|]

setMaxExtent :: Map -> Box -> IO ()
setMaxExtent m box = with box $ \boxPtr ->
  [C.block|void { $fptr-ptr:(Map *m)->set_maximum_extent(*$(bbox *boxPtr)); }|]

insertFontSet :: Map -> FontSetName -> Mapnik.FontSet -> IO ()
insertFontSet m name@(encodeUtf8->n) faceNames = withFontSet name faceNames $ \fs -> [C.block|void {
  $fptr-ptr:(Map *m)->insert_fontset(std::string($bs-ptr:n, $bs-len:n), *$(font_set *fs));
  }|]
  
getFontSetMap :: Map -> IO FontSetMap
getFontSetMap m = do
  ref <- newIORef []
  let cb p = do {v <- peekFontSet p; modifyIORef' ref (v:)}
  [C.safeBlock|void {
    auto const fs = $fptr-ptr:(Map *m)->fontsets();
    for (auto it=fs.begin(); it!=fs.end(); ++it) {
      $fun:(void (*cb)(font_set *))(const_cast<font_set*>(&it->second));
    }
  }|]
  M.fromList <$> readIORef ref

peekFontSet :: Ptr FontSet -> IO (FontSetName, Mapnik.FontSet)
peekFontSet fs = do
  name <- newText "peekFontSet.name" $ \(p,len) -> [C.block|void {
    auto const s = $(font_set *fs)->get_name();
    mallocedString(s, $(char **p), $(int *len));
    }|]
  ref <- newIORef []
  let cb p len = do s <- unsafePackMallocCStringLen (p,fromIntegral len)
                    modifyIORef' ref (s:)
  [C.safeBlock|void  {
    auto const names = $(font_set *fs)->get_face_names();
    for (auto it=names.begin(); it!=names.end(); ++it) {
      char *p;
      int len;
      mallocedString(*it, &p, &len);
      $fun:(void(*cb)(char *, int))(p, len);
    }}|]
  (,) <$> pure name <*> (mapM (decodeUtf8Ctx "peekFontSet") . reverse =<< readIORef ref)

registerFonts :: Map -> FilePath -> IO Bool
registerFonts m (encodeUtf8 . fromString -> d) =
  (toEnum . fromIntegral) <$> [C.exp|int { static_cast<int>($fptr-ptr:(Map *m)->register_fonts(std::string($bs-ptr:d, $bs-len:d)))}|]
