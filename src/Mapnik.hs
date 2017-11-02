{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Mapnik (
  registerPluginDir
, registerFontDir
, registerDefaults
, pluginDir
, fontDir
, mapnikCtx
) where

import           Mapnik.Internal
import           Data.String (fromString)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/datasource_cache.hpp>"
C.include "<mapnik/font_engine_freetype.hpp>"

C.using "namespace mapnik"



-- * Config Constants

pluginDir :: FilePath
pluginDir = DEFAULT_INPUT_PLUGIN_DIR

fontDir :: FilePath
fontDir = DEFAULT_FONT_DIR

registerPluginDir :: FilePath -> IO ()
registerPluginDir (fromString -> path) =
  [C.catchBlock|
  std::string path($bs-ptr:path, $bs-len:path);
  mapnik::datasource_cache::instance().register_datasources(path);
  |]

registerFontDir :: FilePath -> IO ()
registerFontDir (fromString -> path) =
  [C.catchBlock|
  std::string path($bs-ptr:path, $bs-len:path);
  mapnik::freetype_engine::register_fonts(path);
  |]
  
registerDefaults :: IO ()
registerDefaults = do
  registerPluginDir pluginDir
  registerFontDir fontDir
