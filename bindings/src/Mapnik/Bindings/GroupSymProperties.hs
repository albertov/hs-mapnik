{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.GroupSymProperties (
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

-- * GroupSymProperties


foreign import ccall "&hs_mapnik_destroy_GroupSymProperties" destroyGroupSymProperties :: FinalizerPtr GroupSymProperties

unsafeNew :: (Ptr (Ptr GroupSymProperties) -> IO ()) -> IO GroupSymProperties
unsafeNew = mkUnsafeNew GroupSymProperties destroyGroupSymProperties

unsafeNewMaybe :: (Ptr (Ptr GroupSymProperties) -> IO ()) -> IO (Maybe GroupSymProperties)
unsafeNewMaybe = mkUnsafeNewMaybe GroupSymProperties destroyGroupSymProperties

create :: Mapnik.GroupSymProperties -> IO GroupSymProperties
create Mapnik.GroupSymProperties{..} = unsafeNew $ \p ->
  [CU.block|void{
    auto ret = std::make_shared<group_symbolizer_properties>();
    *$(group_symbolizer_properties_ptr **p) = new group_symbolizer_properties_ptr(ret);
  }|]

unCreate :: GroupSymProperties -> IO Mapnik.GroupSymProperties
unCreate p = do
  let rules = []
      layout = Mapnik.SimpleRowLayout Nothing --TODO
  return Mapnik.GroupSymProperties{..}
