{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Lens (
  module Mapnik.Symbolizer.Lens
, module Mapnik.Lens
) where

import Mapnik.TH
import qualified Mapnik.Map as Map
import Mapnik.Common
import Mapnik.Layer
import Mapnik.Rule
import Mapnik.Style
import Mapnik.Parameter
import Mapnik.Datasource
import Mapnik.Symbolizer.Lens

import Control.Lens
import qualified Data.HashMap.Strict as M
import Data.Text (Text)
import Data.Word (Word8)

makeMapnikFields ''Layer
makeMapnikFields ''Map.Map
makeMapnikFields ''Rule
makeMapnikFields ''Style

makePrisms ''Value

class HasStyleLst s a | s -> a where
  styleLst :: Lens' s a

instance HasStyleLst Map.Map [(StyleName,Style)] where
  styleLst = lens (M.toList . Map.styles)
                (\s a -> s { Map.styles = M.fromList a}) 

class HasParameters s a | s -> a where
  parameters :: Lens' s a

instance HasParameters Datasource (M.HashMap Text Value) where
  parameters = lens (\(Datasource s) -> s) (const Datasource) 

class HasRed   s a | s -> a where red :: Lens' s a
class HasGreen s a | s -> a where green :: Lens' s a
class HasBlue  s a | s -> a where blue :: Lens' s a
class HasAlpha s a | s -> a where alpha :: Lens' s a

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
