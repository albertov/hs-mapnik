{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Mapnik.Symbolizer.Property where

import Mapnik.Imports
import Mapnik.Common


--TODO
type FontFeatureSettings = ()
type SimplifyAlgorithm = ()
type GroupProperties = ()
type Scaling = ()
type TextPlacements = ()
type Colorizer = ()

data Prop a = Exp Expression
            | Val a
  deriving (Eq, Show, Functor, Generic)
deriveMapnikJSON ''Prop

type PropValue a = Maybe (Prop a)
