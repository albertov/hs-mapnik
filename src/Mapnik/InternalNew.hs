{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mapnik.InternalNew (
  register_datasources
, register_fonts
, register_defaults
) where

import           Mapnik.Internal

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Monoid ((<>))
import           Data.String (fromString)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C

C.context (C.baseCtx <> C.cppCtx <> C.bsCtx)

C.include "<string>"
C.include "<mapnik/datasource_cache.hpp>"
C.include "<mapnik/font_engine_freetype.hpp>"

register_datasources :: FilePath -> MapnikM ()
register_datasources p = tryIO $ do
  let bsP = fromString p
  [C.catchBlock|
    std::string path = std::string($bs-ptr:bsP, $bs-len:bsP);
    mapnik::datasource_cache::instance().register_datasources(path);
  |]

register_fonts :: FilePath -> MapnikM ()
register_fonts p = tryIO $ do
  let bsP = fromString p
  [C.catchBlock|
    std::string path = std::string($bs-ptr:bsP, $bs-len:bsP);
    mapnik::freetype_engine::register_fonts(path);
  |]
  
register_defaults :: IO ()
register_defaults =
  runMapnik_ (register_datasources pluginDir >> register_fonts fontDir)
