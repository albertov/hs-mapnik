{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Bindings.Projection (
  projTransform
, CanTransform (..)
, NumPoints
) where

import           Mapnik (Proj4)
import           Mapnik.Bindings.Types
import qualified Mapnik.Bindings.Cpp as C
import           Mapnik.Bindings.Orphans()

import           Control.Exception (try)
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Marshal.Utils (with)


import           System.IO.Unsafe (unsafePerformIO)


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/box2d.hpp>"
C.include "<mapnik/projection.hpp>"
C.include "<mapnik/proj_transform.hpp>"
C.include "hs_proj_transform.hpp"

C.using "namespace mapnik"
C.using "bbox = box2d<double>"

-- * Projection

foreign import ccall "&hs_mapnik_destroy_ProjTransform" destroyProjTransform :: FinalizerPtr ProjTransform

projTransform :: Proj4 -> Proj4 -> Either String ProjTransform
projTransform (encodeUtf8->src) (encodeUtf8->dst) = unsafePerformIO $ fmap showExc $ try $ do
  ptr <- C.withPtr_ $ \ptr ->
    [C.catchBlock|
    *$(hs_proj_transform **ptr) = new hs_proj_transform($bs-ptr:src, $bs-ptr:dst);
    |]
  ProjTransform <$> newForeignPtr destroyProjTransform ptr
  where
    showExc = either (Left . show @MapnikError) Right

class CanTransform p where
  forward :: ProjTransform -> p -> p
  backward :: ProjTransform -> p -> p

instance CanTransform Box where
  forward p box =
    unsafePerformIO $ with box $ \boxPtr -> C.withPtr_ $ \ret ->
      [C.block|void {
      *$(bbox *ret) = *$(bbox *boxPtr);
      $fptr-ptr:(hs_proj_transform *p)->trans().forward(*$(bbox *ret));
    }|]
  backward p box =
    unsafePerformIO $ with box $ \boxPtr -> C.withPtr_ $ \ret ->
      [C.block|void {
      *$(bbox *ret) = *$(bbox *boxPtr);
      $fptr-ptr:(hs_proj_transform *p)->trans().backward(*$(bbox *ret));
    }|]

type NumPoints = Int
instance CanTransform (Box,NumPoints) where
  forward p (box, n'@(fromIntegral -> n)) =
    (,n') $ unsafePerformIO $ with box $ \boxPtr -> C.withPtr_ $ \ret ->
      [C.block|void {
      *$(bbox *ret) = *$(bbox *boxPtr);
      $fptr-ptr:(hs_proj_transform *p)->trans().forward(*$(bbox *ret), $(int n));
    }|]
  backward p (box, n'@(fromIntegral -> n)) =
    (,n') $ unsafePerformIO $ with box $ \boxPtr -> C.withPtr_ $ \ret ->
      [C.block|void {
      *$(bbox *ret) = *$(bbox *boxPtr);
      $fptr-ptr:(hs_proj_transform *p)->trans().backward(*$(bbox *ret), $(int n));
    }|]
