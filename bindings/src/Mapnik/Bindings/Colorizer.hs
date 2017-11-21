{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.Colorizer (
  unsafeNewColorizer
, createColorizer
, unCreateColorizer
) where

import qualified Mapnik
import qualified Mapnik.Symbolizer as Mapnik
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import qualified Mapnik.Bindings.Cpp as C
import           Mapnik.Bindings.Orphans ()
import           Foreign.ForeignPtr (FinalizerPtr, withForeignPtr)
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr (Ptr, nullPtr)

import           Control.Exception (bracket)
import           Control.Monad (forM_)
import           Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import           Data.Text.Encoding (encodeUtf8)
import           Data.IORef
import           Foreign.Storable (peek)



C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/raster_colorizer.hpp>"
C.include "util.hpp"

C.using "namespace mapnik"

-- * Colorizer


foreign import ccall "&hs_mapnik_destroy_Colorizer" destroyColorizer :: FinalizerPtr Colorizer

unsafeNewColorizer :: (Ptr (Ptr Colorizer) -> IO ()) -> IO Colorizer
unsafeNewColorizer = mkUnsafeNew Colorizer destroyColorizer

createColorizer :: Mapnik.Colorizer -> IO Colorizer
createColorizer Mapnik.Colorizer{..} = unsafeNewColorizer $ \p -> do
  ret <- C.withPtr_ $ \ret -> [C.block|void {
    *$(raster_colorizer_ptr **p) = new raster_colorizer_ptr(
      std::make_shared<raster_colorizer>()
      );
    *$(void **ret) = (*$(raster_colorizer_ptr **p))->get();
  }|]
  forM_ mode $ \(fromIntegral . fromEnum -> m) ->
    [C.block|void {
    raster_colorizer *colorizer =
      static_cast<raster_colorizer*>($(void *ret));
    colorizer->set_default_mode(static_cast<colorizer_mode_enum>($(int m)));
    }|]
  forM_ color $ \c -> with c $ \cPtr ->
    [C.block|void {
    raster_colorizer *colorizer =
      static_cast<raster_colorizer*>($(void *ret));
    colorizer->set_default_color(*$(color *cPtr));
    }|]
  forM_ stops $ \stop -> withStop stop $ \s ->
    [C.block|void {
    raster_colorizer *colorizer =
      static_cast<raster_colorizer*>($(void *ret));
    colorizer->add_stop(*$(colorizer_stop *s));
    }|]

withStop :: Mapnik.Stop -> (Ptr Stop -> IO a) -> IO a
withStop Mapnik.Stop
  { value = (realToFrac -> val)
  , ..
  } fun = with color $ \c -> bracket (alloc c) dealloc enter
  where
    alloc c = C.withPtr_ $ \ret ->
      [C.block|void {
      auto ret = new colorizer_stop($(float val));
      ret->set_color(*$(color *c));
      *$(colorizer_stop **ret) = ret;
      }|]
    dealloc p = [C.exp|void { delete $(colorizer_stop *p) }|]
    enter p = do
      forM_ label $ \(encodeUtf8 -> lbl) ->
        [C.block|void {
        $(colorizer_stop *p)->set_label(std::string($bs-ptr:lbl, $bs-len:lbl));
        }|]
      forM_ mode $ \(fromIntegral . fromEnum -> mode') ->
        [C.block|void {
        $(colorizer_stop *p)->set_mode(static_cast<colorizer_mode_enum>($(int mode')));
        }|]
      fun p

unCreateStop :: Ptr Stop -> IO Mapnik.Stop
unCreateStop p = do
  (  realToFrac -> value
   , colorPtr
   , mMode
   , labelPtr
   , fromIntegral -> labelLen
   ) <- C.withPtrs_ $ \(val,col,mode,ptr,len) ->
    [C.block|void {
      const colorizer_stop &stop = *$(colorizer_stop *p);
      static const colorizer_stop def;
      *$(float *val) = stop.get_value();
      if ( stop.get_mode() != def.get_mode() ) {
        *$(int *mode) = static_cast<int>(stop.get_mode());
      } else {
        *$(int *mode) = -1;
      }
      *$(color **col) = const_cast<color*>(&stop.get_color());
      if (stop.get_label() != def.get_label()) {
        mallocedString(stop.get_label(), $(char **ptr), $(int *len));
      } else {
        *$(char **ptr) = nullptr;
      }
    }|]
  color <- peek colorPtr
  mode <- if mMode < 0
          then return Nothing
          else return (Just (toEnum (fromIntegral mMode)))
  label <- if labelPtr == nullPtr
           then return Nothing
           else Just
            <$> (decodeUtf8Ctx "unCreateStop" =<< unsafePackMallocCStringLen (labelPtr,labelLen))
  return Mapnik.Stop{..}


getStops :: Ptr Colorizer -> IO [Mapnik.Stop]
getStops p = do
  ref <- newIORef []
  let cb :: Ptr Stop -> IO ()
      cb stopPtr = do stop <- unCreateStop stopPtr
                      modifyIORef' ref (stop:)
  [C.safeBlock|void {
    auto stops = (*$(raster_colorizer_ptr *p))->get_stops();
    for (auto it=stops.begin(); it!=stops.end(); it++) {
      $fun:(void (*cb)(colorizer_stop*))(const_cast<colorizer_stop*>(&*it));
    }
  }|]
  reverse <$> readIORef ref

unCreateColorizer :: Colorizer -> IO Mapnik.Colorizer
unCreateColorizer (Colorizer colorizer) = withForeignPtr colorizer $ \p -> do
  (mMode, colorPtr) <- C.withPtrs_ $ \(mode,col) ->
    [C.block|void {
    const raster_colorizer *colorizer =
      (*$(raster_colorizer_ptr *p)).get();
    static const raster_colorizer def;
    if (colorizer->get_default_mode() != def.get_default_mode()) {
      *$(int *mode) =static_cast<int>(
          (*$(raster_colorizer_ptr *p))->get_default_mode()
        );
    } else {
      *$(int *mode) = -1;
    }
    if (colorizer->get_default_color() != def.get_default_color()) {
      *$(color **col) = const_cast<color*>(
          &(*$(raster_colorizer_ptr *p))->get_default_color()
      );
    } else {
      *$(color **col) = nullptr;
    }
    }|]
  color <- if colorPtr == nullPtr
           then return Nothing
           else Just <$> peek colorPtr
  mode <- if mMode < 0
          then return Nothing
          else return (Just (toEnum (fromIntegral mMode)))
  stops <- getStops p
  return Mapnik.Colorizer{..}
