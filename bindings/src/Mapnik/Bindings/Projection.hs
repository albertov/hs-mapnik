{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Mapnik.Bindings.Projection (
  parse
, toText
, projTransform
, CanTransform (..)
) where

import           Mapnik.Bindings
import           Mapnik.Bindings.Util (newText)
import           Mapnik.Bindings.Orphans()

import           Control.Exception (try)
import           Control.Monad ((<=<))
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)
import           Foreign.Marshal.Utils (with)

import qualified Language.C.Inline.Unsafe as CU
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C

import           System.IO.Unsafe (unsafePerformIO)


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/box2d.hpp>"
C.include "<mapnik/projection.hpp>"
C.include "<mapnik/proj_transform.hpp>"

C.using "namespace mapnik"
C.verbatim "typedef box2d<double> bbox;"

-- * Projection


foreign import ccall "&hs_mapnik_destroy_Projection" destroyProjection :: FinalizerPtr Projection

unsafeNew :: (Ptr (Ptr Projection) -> IO ()) -> IO Projection
unsafeNew = fmap Projection . newForeignPtr destroyProjection <=< C.withPtr_

parse :: Text -> Either String Projection
parse (encodeUtf8 -> s) =
  unsafePerformIO $ fmap showExc $ try $ unsafeNew $ \p ->
    [C.catchBlock|*$(projection **p) = new projection(std::string($bs-ptr:s, $bs-len:s));|]
  where
    showExc = either (Left . show @C.CppException) Right

foreign import ccall "&hs_mapnik_destroy_ProjTransform" destroyProjTransform :: FinalizerPtr ProjTransform

projTransform :: Projection -> Projection -> ProjTransform
projTransform src dst = unsafePerformIO $ do
  ptr <- [CU.exp|proj_transform * {
    new proj_transform(*$fptr-ptr:(projection *src), *$fptr-ptr:(projection *dst))
    }|]
  ProjTransform <$> newForeignPtr destroyProjTransform ptr

  
toText :: Projection -> Text
toText pr = unsafePerformIO $ newText $ \(ptr,len) ->
    [CU.block|void {
    std::string const &s = $fptr-ptr:(projection *pr)->params();
    *$(char** ptr) = strdup(s.c_str());
    *$(int* len) = s.length();
    }|]
    
class CanTransform p where
  forward :: ProjTransform -> p -> p
  backward :: ProjTransform -> p -> p

instance CanTransform Box where
  forward p box =
    unsafePerformIO $ with box $ \boxPtr -> C.withPtr_ $ \ret ->
      [CU.block|void {
      auto res = *$(bbox *boxPtr);
      $fptr-ptr:(proj_transform *p)->forward(res);
      *$(bbox *ret) = res;
    }|]
  backward p box =
    unsafePerformIO $ with box $ \boxPtr -> C.withPtr_ $ \ret ->
      [CU.block|void {
      auto res = *$(bbox *boxPtr);
      $fptr-ptr:(proj_transform *p)->backward(res);
      *$(bbox *ret) = res;
    }|]
