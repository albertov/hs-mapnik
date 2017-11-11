{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module Mapnik.Symbolizer.Property where

import Mapnik.Imports
import Mapnik.Common


--TODO
type FontFeatureSettings = ()
type GroupProperties = ()
type Colorizer = ()

data Prop a = Exp Expression
            | Val a
  deriving (Eq, Show, Functor, Generic)
deriveMapnikJSON ''Prop

type PropValue a = Maybe (Prop a)

data TextPlacements = Simple | List | Dummy
  deriving (Eq, Show, Generic)
deriveMapnikJSON ''TextPlacements
