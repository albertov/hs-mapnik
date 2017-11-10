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
import Mapnik.Layer
import Mapnik.Rule
import Mapnik.Style
import Mapnik.Parameter
import Mapnik.Symbolizer.Lens

import Control.Lens
import qualified Data.HashMap.Strict as M

makeMapnikFields ''Layer
makeMapnikFields ''Map.Map
makeMapnikFields ''Rule
makeMapnikFields ''Style
makePrisms ''ParamValue

-- XXX Shim
instance HasStyles Map.Map (M.HashMap StyleName Style) where
  styles = lens (M.fromList . Map._styleLst)
                (\s a -> s { Map._styleLst = M.toList a}) 
