
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.Symbolizer (
  Symbolizer
, create
) where

import qualified Mapnik
import           Mapnik.Bindings
import           Control.Exception
import           Control.Monad (forM_)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/symbolizer.hpp>"
C.include "<mapnik/symbolizer_keys.hpp>"

C.using "namespace mapnik"

--
-- * Symbolizer


foreign import ccall "&hs_mapnik_destroy_Symbolizer" destroySymbolizer :: FinalizerPtr Symbolizer

newSym :: Ptr Symbolizer -> IO Symbolizer
newSym = fmap Symbolizer . newForeignPtr destroySymbolizer

create :: Mapnik.Symbolizer -> IO Symbolizer
create sym = bracket alloc dealloc $ \p -> newSym =<<
  case sym of
    Mapnik.PointSymbolizer{..} -> do
      forM_ _symbolizerOpacity (setOpacity p)
      [C.block|symbolizer *{ new symbolizer(*static_cast<point_symbolizer*>($(symbolizer_base *p)));}|]
    Mapnik.LineSymbolizer{..} -> do
      forM_ _symbolizerOpacity (setOpacity p)
      [C.block|symbolizer *{ new symbolizer(*static_cast<line_symbolizer*>($(symbolizer_base *p)));}|]
  where
    alloc = [C.exp|symbolizer_base * { new symbolizer_base() }|]
    dealloc p = [C.block|void { delete $(symbolizer_base *p);}|]

setOpacity :: Ptr SymbolizerBase -> Double -> IO ()
setOpacity s (realToFrac -> v) =
  [C.block|void {
    $(symbolizer_base *s)->properties[keys::opacity] = $(double v);
  }|]

