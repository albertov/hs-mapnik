{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

#include "mapnik_c_api.h"

module Mapnik.Internal (
    MapnikM
  , Map
  , BBox
  , Image
  , ImageFormat
  , runMapnik
  , runMapnik_
  , createMap
  , withMap
  , pluginDir
  , fontDir
  , load_map
  , load_map_string
  , zoom_all
  , zoom_to_box
  , render_to_file
  , render_to_image
  , set_srs
  , set_buffer_size
  , resize
  , bbox
  , serialize_image
  , image_from_rgba8
  , image_to_rgba8

  , tryIO
) where

import           Control.Exception
import           Control.Monad (liftM, (<=<))
import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString, useAsCString)
import           Data.ByteString.Unsafe (unsafePackMallocCStringLen, unsafeUseAsCString)
import           Foreign.C.String (CString, withCString, peekCString)
import           Foreign.C.Types (CInt)
import           Foreign.ForeignPtr (newForeignPtr)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, nullPtr, castPtr)
import           Foreign.Storable (peek)
import           System.IO.Unsafe (unsafePerformIO)


newtype MapnikM a = MapnikM (EitherT String IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runMapnik :: MapnikM a -> IO (Either String a)
runMapnik (MapnikM a) = runEitherT a

runMapnik_ :: MapnikM a -> IO a
runMapnik_ = either fail return <=< runMapnik


-- * Config Constants

pluginDir :: FilePath
pluginDir = DEFAULT_INPUT_PLUGIN_DIR

fontDir :: FilePath
fontDir = DEFAULT_FONT_DIR

-- * Initialization

register_fonts :: FilePath -> MapnikM ()
register_fonts path =
  checkError "register_fonts" $ \errPtr ->
  withCString path $ \cPath ->
    {#call mapnik_register_fonts #} cPath errPtr


-- * Image

{#pointer *mapnik_image_t as Image foreign finalizer mapnik_image_free newtype #}

type ImageFormat = String

showImageFormat :: ImageFormat -> String
showImageFormat = id


serialize_image :: ImageFormat -> Image -> ByteString
serialize_image (showImageFormat -> format) im = unsafePerformIO $
  withCString format $ \fmtPtr ->
  withImage im $ \imPtr ->
  alloca $ \lenPtr -> do
    ptr <- {#call mapnik_image_data#} imPtr fmtPtr lenPtr
    if ptr == nullPtr then return mempty else do
      len <- peek lenPtr
      unsafePackMallocCStringLen (ptr, fromIntegral len)


-- * BBox

{#pointer *mapnik_bbox_t as BBox foreign finalizer mapnik_bbox_free newtype #}

bbox :: Double -> Double -> Double -> Double -> BBox
bbox (realToFrac -> a) (realToFrac -> b) (realToFrac -> c) (realToFrac -> d)
  = unsafePerformIO $ do
      p <- {#call unsafe mapnik_bbox #} a b c d
      liftM BBox (newForeignPtr mapnik_bbox_free p)


-- * Map

{#pointer *mapnik_map_t as Map foreign finalizer mapnik_map_free newtype #}

createMap :: Int -> Int -> MapnikM Map
createMap (fromIntegral -> w) (fromIntegral -> h) = runIO $ do
  mPtr <- newForeignPtr mapnik_map_free =<< {#call unsafe mapnik_map #} w h
  return (Right (Map mPtr))

set_srs :: Map -> String -> MapnikM ()
set_srs m srs =
  checkMapError "set_srs" m $ \mPtr ->
  withCString srs ({#call unsafe mapnik_map_set_srs #} mPtr)

set_buffer_size :: Map -> Int -> MapnikM ()
set_buffer_size m (fromIntegral -> size) = runIO $ withMap m $ \mPtr -> do
  {#call unsafe mapnik_map_set_buffer_size #} mPtr size
  return (Right ())

resize :: Map -> Int -> Int -> MapnikM ()
resize m (fromIntegral -> width) (fromIntegral -> height) =
  runIO $ withMap m $ \mPtr -> do
    {#call unsafe mapnik_map_resize #} mPtr width height
    return (Right ())

load_map :: Map -> FilePath -> MapnikM ()
load_map m path =
  checkMapError "load_map" m $ \mPtr ->
  withCString path ({#call mapnik_map_load #} mPtr)

load_map_string :: Map -> ByteString -> MapnikM ()
load_map_string m xml =
  checkMapError "load_map_string" m $ \mPtr ->
  useAsCString xml ({#call mapnik_map_load_string #} mPtr)

render_to_file :: Map -> FilePath -> Double -> MapnikM ()
render_to_file m path scaleFactor =
  checkMapError "render_to_file" m $ \mPtr ->
  withCString path $ \pPtr ->
    {#call mapnik_map_render_to_file #} mPtr pPtr (realToFrac scaleFactor)

render_to_image :: Map -> Double -> MapnikM Image
render_to_image m scaleFactor = runIO $ withMap m $ \mPtr -> do
  iPtr <- {#call mapnik_map_render_to_image#} mPtr (realToFrac scaleFactor)
  if iPtr == nullPtr
    then liftM (Left . ("render_to_image: " ++))
               (peekCString =<< {#call unsafe mapnik_map_last_error#} mPtr)
    else liftM (Right . Image) (newForeignPtr mapnik_image_free iPtr)

zoom_all :: Map -> MapnikM ()
zoom_all m = checkMapError "zoom_all" m {#call mapnik_map_zoom_all #}

zoom_to_box :: Map -> BBox -> MapnikM ()
zoom_to_box m b = runIO $
  withMap m $ \mPtr ->
  withBBox b $ \bPtr -> do
    {#call unsafe mapnik_map_zoom_to_box #} mPtr bPtr
    return (Right ())

image_to_rgba8 :: Image -> ByteString
image_to_rgba8 im = unsafePerformIO $
  withImage im $ \imPtr ->
  alloca $ \lenPtr -> do
    ptr <- {#call unsafe mapnik_image_rgba8_data#} imPtr lenPtr
    if ptr == nullPtr then return mempty else do
      len <- peek lenPtr
      unsafePackMallocCStringLen (ptr, fromIntegral len)
{-# NOINLINE image_to_rgba8 #-}

image_from_rgba8 :: Int -> Int -> ByteString -> Maybe Image
image_from_rgba8 w h rgba8
  | w*h*4 /= BS.length rgba8 = Nothing
  | otherwise = unsafePerformIO $ unsafeUseAsCString rgba8 $ \rgba8Ptr -> do
    iPtr <- {#call unsafe mapnik_image_from_rgba8_data#}
            (fromIntegral w) (fromIntegral h) (castPtr rgba8Ptr)
    if iPtr == nullPtr
      then pure Nothing
      else Just . Image <$> newForeignPtr mapnik_image_free iPtr
{-# NOINLINE image_from_rgba8 #-}

{-# RULES
"image_from_rgba8/image_to_rgba8"  forall w h im. image_from_rgba8 w h (image_to_rgba8 im) = Just im
  #-}

-- * Internal Utils

runIO :: IO (Either String a) -> MapnikM a
runIO a = MapnikM (hoistEither =<< liftIO a)

tryIO :: IO a -> MapnikM a
tryIO a = MapnikM (hoistEither =<< fmap showExc (liftIO (try a)))
  where
    showExc = either (Left . show @SomeException) Right 

checkError :: String -> (Ptr CString -> IO CInt) -> MapnikM ()
checkError prefix act = runIO $ alloca $ \errPtr -> do
  ret <- act errPtr
  if ret == 0 then return (Right ())
    else liftM (Left . ((prefix ++ ": ") ++)) (peekCString =<< peek errPtr)

checkMapError :: String -> Map -> (Ptr Map -> IO CInt) -> MapnikM ()
checkMapError prefix map_ act = runIO $ withMap map_ $ \mPtr -> do
  ret <- act mPtr
  if ret == 0 then return (Right ())
    else liftM (Left . ((prefix ++ ": ") ++))
               (peekCString =<< {#call unsafe mapnik_map_last_error#} mPtr)
