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
import Mapnik.Datasource
import Mapnik.Symbolizer.Lens

import Control.Lens
import qualified Data.HashMap.Strict as M
import Data.Text (Text)

makeMapnikFields ''Layer
makeMapnikFields ''Map.Map
makeMapnikFields ''Rule
makeMapnikFields ''Style

makePrisms ''ParamValue

class HasStyleLst s a | s -> a where
  styleLst :: Lens' s a

instance HasStyleLst Map.Map [(StyleName,Style)] where
  styleLst = lens (M.toList . Map.styles)
                (\s a -> s { Map.styles = M.fromList a}) 

class HasParameters s a | s -> a where
  parameters :: Lens' s a

instance HasParameters Datasource (M.HashMap Text ParamValue) where
  parameters = lens (\(Datasource s) -> s) (const Datasource) 
