
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.Symbolizer (
  Symbolizer
, unsafeNew
) where

import           Mapnik.Bindings
import           Control.Monad ((<=<))
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/symbolizer.hpp>"

C.using "namespace mapnik"

--
-- * Symbolizer


foreign import ccall "&hs_mapnik_destroy_Symbolizer" destroySymbolizer :: FinalizerPtr Symbolizer

unsafeNew :: (Ptr (Ptr Symbolizer) -> IO ()) -> IO Symbolizer
unsafeNew = fmap Symbolizer . newForeignPtr destroySymbolizer <=< C.withPtr_
