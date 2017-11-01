{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mapnik.Internal (
  Map
, Box (..)
, Image
, createMap
, load_map
, load_map_string
, zoom_all
, zoom_to_box
, render_to_image
, set_srs
, set_buffer_size
, resize
, serialize_image
, image_from_rgba8
, image_to_rgba8

, register_datasources
, register_fonts
, register_defaults
, pluginDir
, fontDir
) where

import           Data.Monoid ((<>))
import           Data.String (fromString)
import           Data.ByteString (ByteString)
import           Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import           Foreign.ForeignPtr (ForeignPtr, FinalizerPtr, newForeignPtr)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C

import           System.IO.Unsafe (unsafePerformIO)

C.context (C.baseCtx <> C.cppCtx <> C.bsCtx <> C.fptrCtx)

C.include "<string>"
C.include "<mapnik/agg_renderer.hpp>"
C.include "<mapnik/image.hpp>"
C.include "<mapnik/image_util.hpp>"
C.include "<mapnik/map.hpp>"
C.include "<mapnik/load_map.hpp>"
C.include "<mapnik/datasource_cache.hpp>"
C.include "<mapnik/font_engine_freetype.hpp>"

-- * Map

newtype Map = Map (ForeignPtr ())

foreign import ccall "&hs_mapnik_destroy_Map" destroyMap :: FinalizerPtr ()

createMap :: Int -> Int -> IO Map
createMap (fromIntegral -> width) (fromIntegral -> height) = do
  ptr <- C.withPtr_ $ \resPtr ->
    [C.catchBlock|
    *$(void** resPtr) = new mapnik::Map($(int width), $(int height));
    |]
  Map <$> newForeignPtr destroyMap ptr

load_map :: Map -> FilePath -> IO ()
load_map m (fromString -> path) =
  [C.catchBlock|
  mapnik::Map *m = static_cast<mapnik::Map*>($fptr-ptr:(void *m));
  mapnik::load_map(*m, std::string($bs-ptr:path, $bs-len:path));
  |]

load_map_string :: Map -> ByteString -> IO ()
load_map_string m str =
  [C.catchBlock|
  mapnik::Map *m = static_cast<mapnik::Map*>($fptr-ptr:(void *m));
  mapnik::load_map_string(*m, std::string($bs-ptr:str, $bs-len:str));
  |]
  
set_srs :: Map -> String -> IO ()
set_srs m (fromString -> srs) =
  [C.catchBlock|
  mapnik::Map *m = static_cast<mapnik::Map*>($fptr-ptr:(void *m));
  m->set_srs(std::string($bs-ptr:srs, $bs-len:srs));
  |]

set_buffer_size :: Map -> Int -> IO ()
set_buffer_size m (fromIntegral -> size) =
  [C.catchBlock|
  mapnik::Map *m = static_cast<mapnik::Map*>($fptr-ptr:(void *m));
  m->set_buffer_size($(int size));
  |]

resize :: Map -> Int -> Int -> IO ()
resize m (fromIntegral -> width) (fromIntegral -> height) =
  [C.catchBlock|
  mapnik::Map *m = static_cast<mapnik::Map*>($fptr-ptr:(void *m));
  m->resize($(int width), $(int height));
  |]

zoom_all :: Map -> IO ()
zoom_all m = 
  [C.catchBlock|
  mapnik::Map *m = static_cast<mapnik::Map*>($fptr-ptr:(void *m));
  m->zoom_all();
  |]

data Box = Box { x0, y0, x1, y1 :: {-# UNPACK #-}!Double }
  deriving (Eq, Show)

zoom_to_box :: Map -> Box -> IO ()
zoom_to_box m (Box (realToFrac -> x0) (realToFrac -> y0) (realToFrac -> x1) (realToFrac -> y1)) = 
  [C.catchBlock|
  mapnik::Map *m = static_cast<mapnik::Map*>($fptr-ptr:(void *m));
  m->zoom_to_box(mapnik::box2d<double>($(double x0), $(double y0), $(double x1), $(double y1)));
  |]

-- * Image

newtype Image = Image (ForeignPtr ())

foreign import ccall "&hs_mapnik_destroy_Image" destroyImage :: FinalizerPtr ()

render_to_image :: Map -> Double -> IO Image
render_to_image m (realToFrac -> scale) = do
  ptr <- C.withPtr_ $ \resPtr ->
    [C.catchBlock|
    mapnik::Map *m = static_cast<mapnik::Map*>($fptr-ptr:(void *m));
    mapnik::image_rgba8 *im = new mapnik::image_rgba8(m->width(), m->height());
    try {
      mapnik::agg_renderer<mapnik::image_rgba8> ren(*m, *im, $(double scale));
      ren.apply();
      *$(void** resPtr) = im;
    } catch (...) {
      delete im;
      throw;
    }
    |]
  Image <$> newForeignPtr destroyImage ptr

serialize_image :: String -> Image -> ByteString
serialize_image (fromString -> fmt) im = unsafePerformIO $ do
  (ptr,len) <- C.withPtrs_ $ \(ptr, len) -> do
    [C.catchBlock|
    mapnik::image_rgba8 *im = static_cast<mapnik::image_rgba8*>($fptr-ptr:(void *im));
    std::string fmt = std::string($bs-ptr:fmt, $bs-len:fmt);
    std::string s = save_to_string(*im, fmt);
    if (! s.length() ) {
      throw std::runtime_error("could not serialize image");
    }
    *$(char** ptr) = static_cast<char*>(malloc(s.length()));
    if ( ! *$(char** ptr) ) {
      throw std::runtime_error("Could not malloc");
    }
    memcpy( *$(char** ptr), s.c_str(), s.length());
    *$(int* len) = s.length();
    |]
  unsafePackMallocCStringLen (ptr, fromIntegral len)


image_to_rgba8 :: Image -> ByteString
image_to_rgba8 im = unsafePerformIO $ do
  (ptr,len) <- C.withPtrs_ $ \(ptr, len) ->
    [C.catchBlock|
    mapnik::image_rgba8 *im = static_cast<mapnik::image_rgba8*>($fptr-ptr:(void *im));
    int len = *$(int* len) = im->size();
    if (len <= 0) {
      throw std::runtime_error("Invalid image size");
    }
    *$(char** ptr) = static_cast<char*>(malloc(len));
    if ( ! *$(char** ptr) ) {
      throw std::runtime_error("Could not malloc");
    }
    memcpy(*$(char** ptr), im->data(), len);
    |]
  unsafePackMallocCStringLen (ptr, fromIntegral len)
{-# NOINLINE image_to_rgba8 #-}

image_from_rgba8 :: Int -> Int -> ByteString -> Maybe Image
image_from_rgba8 (fromIntegral -> width) (fromIntegral -> height) rgba8 = unsafePerformIO $ do
  ptr <- [C.exp|void * {
    new mapnik::image_rgba8($(int width), $(int height), reinterpret_cast<unsigned char*>($bs-ptr:rgba8))
    }|]
  Just . Image <$> newForeignPtr destroyImage ptr
{-# NOINLINE image_from_rgba8 #-}
  
{-# RULES
"image_from_rgba8/image_to_rgba8"  forall w h im. image_from_rgba8 w h (image_to_rgba8 im) = Just im
  #-}

-- * Config Constants

pluginDir :: FilePath
pluginDir = DEFAULT_INPUT_PLUGIN_DIR

fontDir :: FilePath
fontDir = DEFAULT_FONT_DIR

register_datasources :: FilePath -> IO ()
register_datasources (fromString -> path) =
  [C.catchBlock|
    std::string path($bs-ptr:path, $bs-len:path);
    mapnik::datasource_cache::instance().register_datasources(path);
  |]

register_fonts :: FilePath -> IO ()
register_fonts (fromString -> path) =
  [C.catchBlock|
    std::string path($bs-ptr:path, $bs-len:path);
    mapnik::freetype_engine::register_fonts(path);
  |]
  
register_defaults :: IO ()
register_defaults =
  register_datasources pluginDir >> register_fonts fontDir
