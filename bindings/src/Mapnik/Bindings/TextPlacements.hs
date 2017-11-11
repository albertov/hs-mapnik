{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import qualified Mapnik.Bindings.TextSymProperties as Props
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/symbolizer_base.hpp>"
C.include "<mapnik/text/placements/dummy.hpp>"

C.using "namespace mapnik"

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
  [C.block|void {
    auto placements = std::make_shared<text_placements_dummy>();
    *$(text_placements_ptr **p) = new text_placements_ptr(placements);
    placements->defaults = *$fptr-ptr:(text_symbolizer_properties *defaults);
  }|]

--TODO
unCreate :: TextPlacements -> IO Mapnik.TextPlacements
unCreate = const (return Mapnik.dummyPlacements)
