{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.GroupProperties (
  unsafeNew
, unsafeNewMaybe
, create
, unCreate
) where

import qualified Mapnik
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

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

create :: Mapnik.GroupProperties -> IO GroupProperties
create Mapnik.GroupProperties = unsafeNew $ \p ->
  [CU.block|void{
    auto ret = std::make_shared<group_symbolizer_properties>();
    *$(group_symbolizer_properties_ptr **p) = new group_symbolizer_properties_ptr(ret);
  }|]

unCreate :: GroupProperties -> IO Mapnik.GroupProperties
unCreate p = return Mapnik.GroupProperties
