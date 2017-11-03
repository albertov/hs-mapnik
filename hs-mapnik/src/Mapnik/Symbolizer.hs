{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Symbolizer where

import Mapnik.Imports

data Symbolizer
  = PointSymbolizer
    { _symbolizerOpacity :: !(Maybe Double)
    }
  | LineSymbolizer
    { _symbolizerOpacity :: !(Maybe Double)
    }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

makeClassy ''Symbolizer
makeFields ''Symbolizer
makePrisms ''Symbolizer
