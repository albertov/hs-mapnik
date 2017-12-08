{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Bindings.VectorTile.Lens where

import Mapnik.Bindings.VectorTile.Render
import Mapnik.Lens
import Mapnik.TH

makeMapnikFields ''RenderSettings
