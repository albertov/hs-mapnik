{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Symbolizer.Lens where

import Mapnik.TH
import Mapnik.Symbolizer

import Control.Lens

type ApLens' s a = forall f . Applicative f => (a -> f a) -> s -> f s

class HasAllowOverlap s a | s -> a where allowOverlap :: ApLens' s a
class HasFill s a | s -> a where fill :: ApLens' s a
class HasAvoidEdges s a | s -> a where avoidEdges :: ApLens' s a
class HasDirection s a | s -> a where direction :: ApLens' s a
class HasDx s a | s -> a where dx :: ApLens' s a
class HasDy s a | s -> a where dy :: ApLens' s a

makeMapnikFields ''TextProperties
makeMapnikFields ''FormatProperties
makeMapnikFields ''TextLayoutProperties
makeMapnikFields ''Symbolizer
makePrisms ''Symbolizer
makePrisms ''Prop
