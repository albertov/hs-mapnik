
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Mapnik.Bindings.GroupProperties (
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
C.include "<mapnik/symbolizer_base.hpp>"

C.using "namespace mapnik"

-- * GroupProperties


foreign import ccall "&hs_mapnik_destroy_GroupProperties" destroyGroupProperties :: FinalizerPtr GroupProperties

unsafeNew :: (Ptr (Ptr GroupProperties) -> IO ()) -> IO GroupProperties
unsafeNew = mkUnsafeNew GroupProperties destroyGroupProperties

unsafeNewMaybe :: (Ptr (Ptr GroupProperties) -> IO ()) -> IO (Maybe GroupProperties)
unsafeNewMaybe = mkUnsafeNewMaybe GroupProperties destroyGroupProperties

create :: Mapnik.GroupProperties -> IO Colorizer
create = undefined

unCreate :: GroupProperties -> IO Mapnik.GroupProperties
unCreate = undefined
