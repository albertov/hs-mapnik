{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Lens (
  module Mapnik.Symbolizer.Lens
, module Mapnik.Lens
) where

import Mapnik.TH
import Mapnik.Map
import Mapnik.Layer
import Mapnik.Rule
import Mapnik.Style
import Mapnik.Parameter
import Mapnik.Symbolizer.Lens

import Control.Lens

makeMapnikFields ''Layer
makeMapnikFields ''Map
makeMapnikFields ''Rule
makeMapnikFields ''Style
makePrisms ''ParamValue
