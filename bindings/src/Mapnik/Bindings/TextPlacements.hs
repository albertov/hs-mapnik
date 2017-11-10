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
) where

import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C



C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/transform_expression.hpp>"
C.include "<mapnik/parse_transform.hpp>"
C.include "<mapnik/transform_processor.hpp>"

C.using "namespace mapnik"

-- * TextPlacements


foreign import ccall "&hs_mapnik_destroy_TextPlacements" destroyTextPlacements :: FinalizerPtr TextPlacements

unsafeNew :: (Ptr (Ptr TextPlacements) -> IO ()) -> IO TextPlacements
unsafeNew = mkUnsafeNew TextPlacements destroyTextPlacements

unsafeNewMaybe :: (Ptr (Ptr TextPlacements) -> IO ()) -> IO (Maybe TextPlacements)
unsafeNewMaybe = mkUnsafeNewMaybe TextPlacements destroyTextPlacements
