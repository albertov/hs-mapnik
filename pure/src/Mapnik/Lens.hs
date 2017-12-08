{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Lens where

import Mapnik.TH
import qualified Mapnik.Map as Map
import Mapnik.Common hiding (maxx,maxy,minx,miny)
import Mapnik.Layer
import Mapnik.Color
import Mapnik.ImageFilter
import Mapnik.Rule
import Mapnik.Style
import Mapnik.Parameter
import Mapnik.Datasource
import Mapnik.Symbolizer

import Control.Lens
import qualified Data.HashMap.Strict as M
import Data.Bits
import Data.Text (Text)
import Data.Word (Word8)


class HasStyleLst     s a | s -> a where styleLst :: Lens' s a
class HasParameters   s a | s -> a where parameters :: Lens' s a
class HasRed          s a | s -> a where red :: Lens' s a
class HasGreen        s a | s -> a where green :: Lens' s a
class HasBlue         s a | s -> a where blue :: Lens' s a
class HasAlpha        s a | s -> a where alpha :: Lens' s a

-- Needs to be declared First so Traversal's are created instead of Lens'es
makeMapnikFields ''Symbolizer

makeMapnikFields ''Layer
makeMapnikFields ''Map.Map
makeMapnikFields ''Rule
makeMapnikFields ''Style
makeMapnikFields ''Format
makeMapnikFields ''TextProperties
makeMapnikFields ''TextFormatProperties
makeMapnikFields ''TextLayoutProperties
makeMapnikFields ''TextSymProperties
makeMapnikFields ''GroupSymProperties
makeMapnikFields ''GroupRule
makeMapnikFields ''GroupLayout
makeMapnikFields ''Colorizer
makeMapnikFields ''Stop
makeMapnikFields ''Box
makeMapnikFields ''ImageRgba8
makePrisms ''Value
makePrisms ''Symbolizer
makePrisms ''Prop
makePrisms ''Format
makePrisms ''GroupLayout
makePrisms ''Font
makePrisms ''ImageFilter


--------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------

instance HasStyleLst Map.Map [(StyleName,Style)] where
  styleLst = lens (M.toList . Map.styles)
                  (\s a -> s { Map.styles = M.fromList a})

instance HasParameters Datasource (M.HashMap Text Value) where
  parameters = lens (\(Datasource s) -> s) (const Datasource)

instance HasRed Color Word8 where
  red   = lens (\(RGBA r _ _ _) -> r) (\(RGBA _ g b a) r -> RGBA r g b a)
  {-# INLINE red #-}
instance HasGreen Color Word8 where
  green = lens (\(RGBA _ g _ _) -> g) (\(RGBA r _ b a) g -> RGBA r g b a)
  {-# INLINE green #-}
instance HasBlue Color Word8 where
  blue  = lens (\(RGBA _ _ b _) -> b) (\(RGBA r g _ a) b -> RGBA r g b a)
  {-# INLINE blue #-}
instance HasAlpha Color Word8 where
  alpha = lens (\(RGBA _ _ _ a) -> a) (\(RGBA r g b _) a -> RGBA r g b a)
  {-# INLINE alpha #-}

instance HasWidth Box Double where
  width = lens (\b -> b^.maxx - b^.minx) (\b w -> b & maxx .~ (b^.minx + w))
  {-# INLINE width #-}

instance HasHeight Box Double where
  height = lens (\b -> b^.maxy - b^.miny) (\b h -> b & maxy .~ (b^.miny + h))
  {-# INLINE height #-}

#define COLOR_LENS(cname, name, off) \
instance cname PixelRgba8 Word8 where {\
  {-# INLINE name #-}; \
  name = lens \
    (fromIntegral . (.&. 0xFF) . (`unsafeShiftR` off) . unRgba8) \
    (\(PixelRgba8 c) v -> PixelRgba8 (c .&. complement (0xFF `unsafeShiftL` off) .|. (fromIntegral v `unsafeShiftL` off))) \
  };

COLOR_LENS(HasRed,   red,    0)
COLOR_LENS(HasGreen, green,  8)
COLOR_LENS(HasBlue,  blue,  16)
COLOR_LENS(HasAlpha, alpha, 24)
