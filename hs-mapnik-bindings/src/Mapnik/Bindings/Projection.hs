{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Mapnik.Bindings.Projection (
  fromProj4
, transform
, Transform (..)
) where

import           Mapnik.Bindings
import           Mapnik.Bindings.Util (newByteString)
import           Data.ByteString (unpack)
import           Data.Char (chr)
import           Control.Exception (try)
import           Control.Monad ((<=<))
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C

import           System.IO.Unsafe (unsafePerformIO)


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/box2d.hpp>"
C.include "<mapnik/projection.hpp>"
C.include "<mapnik/proj_transform.hpp>"

C.using "namespace mapnik"

-- * Projection


foreign import ccall "&hs_mapnik_destroy_Projection" destroyProjection :: FinalizerPtr Projection

unsafeNew :: (Ptr (Ptr Projection) -> IO ()) -> IO Projection
unsafeNew = fmap Projection . newForeignPtr destroyProjection <=< C.withPtr_

fromProj4 :: Text -> Either String Projection
fromProj4 (encodeUtf8 -> s) =
  unsafePerformIO $ fmap showExc $ try $ unsafeNew $ \p ->
    [C.catchBlock|*$(projection **p) = new projection(std::string($bs-ptr:s, $bs-len:s));|]
  where
    showExc = either (Left . show @C.CppException) Right

foreign import ccall "&hs_mapnik_destroy_ProjTransform" destroyProjTransform :: FinalizerPtr ProjTransform

transform :: Projection -> Projection -> ProjTransform
transform src dst = unsafePerformIO $ do
  ptr <- [C.exp|proj_transform * {
    new proj_transform(*$fptr-ptr:(projection *src), *$fptr-ptr:(projection *dst))
    }|]
  ProjTransform <$> newForeignPtr destroyProjTransform ptr

  
instance Show Projection where
  show pr = unsafePerformIO $ fmap (map (chr . fromIntegral) . unpack) $ newByteString $ \(ptr,len) ->
    [C.catchBlock|
    std::string const &s = $fptr-ptr:(projection *pr)->params();
    *$(char** ptr) = static_cast<char*>(malloc(s.length()));
    if ( ! *$(char** ptr) ) {
      throw std::runtime_error("Could not malloc");
    }
    memcpy( *$(char** ptr), s.c_str(), s.length());
    *$(int* len) = s.length();
    |]
    
class Transform p where
  forward :: ProjTransform -> p -> p
  backward :: ProjTransform -> p -> p

newBox ::((Ptr C.CDouble, Ptr C.CDouble, Ptr C.CDouble, Ptr C.CDouble) -> IO ()) -> IO Box
newBox fun = do
  (x0,y0,x1,y1) <- C.withPtrs_ fun
  return (Box (realToFrac x0) (realToFrac y0) (realToFrac x1) (realToFrac y1))

instance Transform Box where
  forward p (Box (realToFrac->x0) (realToFrac->y0) (realToFrac->x1) (realToFrac->y1)) =
    unsafePerformIO $ newBox $ \(px0,py0,px1,py1) ->
      [C.block|void {
      box2d<double> res($(double x0), $(double y0), $(double x1), $(double y1));
      $fptr-ptr:(proj_transform *p)->forward(res);
      *$(double *px0) = res.minx();
      *$(double *py0) = res.miny();
      *$(double *px1) = res.maxx();
      *$(double *py1) = res.maxy();
    }|]
  backward p (Box (realToFrac->x0) (realToFrac->y0) (realToFrac->x1) (realToFrac->y1)) =
    unsafePerformIO $ newBox $ \(px0,py0,px1,py1) ->
      [C.block|void {
      box2d<double> res($(double x0), $(double y0), $(double x1), $(double y1));
      $fptr-ptr:(proj_transform *p)->backward(res);
      *$(double *px0) = res.minx();
      *$(double *py0) = res.miny();
      *$(double *px1) = res.maxx();
      *$(double *py1) = res.maxy();
    }|]
