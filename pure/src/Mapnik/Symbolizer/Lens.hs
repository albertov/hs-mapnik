{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Symbolizer.Lens where

import Mapnik.TH
import Mapnik.Symbolizer

import Control.Lens

class HasAllowOverlap s a | s -> a where allowOverlap :: Traversal' s a
class HasFill         s a | s -> a where fill         :: Traversal' s a
class HasAvoidEdges   s a | s -> a where avoidEdges   :: Traversal' s a
class HasDirection    s a | s -> a where direction    :: Traversal' s a
class HasDx           s a | s -> a where dx           :: Traversal' s a
class HasDy           s a | s -> a where dy           :: Traversal' s a

makeMapnikFields ''Format
makeMapnikFields ''TextProperties
makeMapnikFields ''TextFormatProperties
makeMapnikFields ''TextLayoutProperties
makeMapnikFields ''TextSymProperties
makeMapnikFields ''Symbolizer
makeMapnikFields ''Colorizer
makeMapnikFields ''Stop
makePrisms ''Symbolizer
makePrisms ''Prop
makePrisms ''Format
