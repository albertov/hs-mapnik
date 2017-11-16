{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Mapnik.Bindings.TextPlacements (
  unsafeNew
, unsafeNewMaybe
, create
, unCreate
) where

import qualified Mapnik
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Variant (Variant(..), justOrTypeError)
import qualified Mapnik.Bindings.TextSymProperties as Props
import           Control.Exception (throwIO)
import           Control.Monad (when)
import           Foreign.ForeignPtr (FinalizerPtr, withForeignPtr)
import           Foreign.Ptr (Ptr, nullPtr)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/symbolizer_base.hpp>"
C.include "<mapnik/text/placements/base.hpp>"
C.include "<mapnik/text/placements/dummy.hpp>"
C.include "<mapnik/text/placements/simple.hpp>"
C.include "<mapnik/text/placements/list.hpp>"

C.using "namespace mapnik"
C.verbatim "typedef symbolizer_base::value_type sym_value_type;"

-- * TextPlacements


foreign import ccall "&hs_mapnik_destroy_TextPlacements" destroyTextPlacements :: FinalizerPtr TextPlacements

unsafeNew :: (Ptr (Ptr TextPlacements) -> IO ()) -> IO TextPlacements
unsafeNew = mkUnsafeNew TextPlacements destroyTextPlacements

unsafeNewMaybe :: (Ptr (Ptr TextPlacements) -> IO ()) -> IO (Maybe TextPlacements)
unsafeNewMaybe = mkUnsafeNewMaybe TextPlacements destroyTextPlacements

--TODO
create :: Mapnik.TextPlacements -> IO TextPlacements
create (Mapnik.Dummy defs) = unsafeNew $ \p -> do
  defaults <- Props.create defs
  [CU.block|void {
    auto placements = std::make_shared<text_placements_dummy>();
    *$(text_placements_ptr **p) = new text_placements_ptr(placements);
    placements->defaults = *$fptr-ptr:(text_symbolizer_properties *defaults);
  }|]

unCreate :: TextPlacements -> IO Mapnik.TextPlacements
unCreate (TextPlacements fp) = withForeignPtr fp $ \p -> do
  ptr <- C.withPtr_ $ \ptr -> [CU.block|void {
    // Cannot dynamic_cast on simple or list placements since they're not exposed
    *$(text_placements **ptr) =
      dynamic_cast<text_placements_dummy *>($(text_placements_ptr *p)->get());
    }|]
  when (ptr == nullPtr) $
    throwIO (userError "Unsupported placement type")
  fmap Mapnik.Dummy . Props.unCreate =<< [CU.exp|text_symbolizer_properties * {
    // Assumes p is not null since ptr isnt
    &$(text_placements *ptr)->defaults
  }|]


instance Variant SymbolizerValue Mapnik.TextPlacements where
  peekV p = unCreate =<<
    justOrTypeError (unsafeNewMaybe $ \ret ->
      [CU.block|void {
      try {
        *$(text_placements_ptr **ret) =
          new text_placements_ptr(util::get<text_placements_ptr>(*$(sym_value_type *p)));
      } catch (std::exception) {
        *$(text_placements_ptr **ret) = nullptr;
      }
      }|])
  pokeV p v' = do
    v <- create v'
    [CU.block|void { *$(sym_value_type *p) = sym_value_type(*$fptr-ptr:(text_placements_ptr *v)); }|]
