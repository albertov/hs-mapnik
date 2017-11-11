{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Mapnik.Bindings.Colorizer (
  unsafeNew
, unsafeNewMaybe
, create
, unCreate
) where

import qualified Mapnik
import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C



C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/raster_colorizer.hpp>"

C.using "namespace mapnik"

-- * Colorizer


foreign import ccall "&hs_mapnik_destroy_Colorizer" destroyColorizer :: FinalizerPtr Colorizer

unsafeNew :: (Ptr (Ptr Colorizer) -> IO ()) -> IO Colorizer
unsafeNew = mkUnsafeNew Colorizer destroyColorizer

unsafeNewMaybe :: (Ptr (Ptr Colorizer) -> IO ()) -> IO (Maybe Colorizer)
unsafeNewMaybe = mkUnsafeNewMaybe Colorizer destroyColorizer

create :: Mapnik.Colorizer -> IO Colorizer
create = undefined

unCreate :: Colorizer -> IO Mapnik.Colorizer
unCreate = undefined
