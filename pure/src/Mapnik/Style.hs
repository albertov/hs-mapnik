{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Style where

import Mapnik.Imports
import Mapnik.Rule
import Data.Default
import Data.Text

type StyleName = Text

type Styles = [(StyleName, Style)]

data Style = Style
  { opacity             :: !(Maybe Double)
  , imageFiltersInflate :: !(Maybe Bool)
  , rules               :: ![Rule]
  } deriving (Eq, Show, Generic, Default)
deriveMapnikJSON ''Style
