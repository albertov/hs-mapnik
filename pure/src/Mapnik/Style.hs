{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Style where

import Mapnik.Imports
import Mapnik.ImageFilter
import Mapnik.Enums
import Mapnik.Rule
import Data.Default
import Data.Text
import qualified Data.HashMap.Strict as M

type StyleName = Text

type Styles = M.HashMap StyleName Style

data Style = Style
  { opacity             :: !(Maybe Double)
  , filterMode          :: !(Maybe FilterMode)
  , filters             :: ![ImageFilter]
  , directFilters       :: ![ImageFilter]
  , imageFiltersInflate :: !(Maybe Bool)
  , rules               :: ![Rule]
  , compOp              :: !(Maybe CompositeMode)
  } deriving (Eq, Show, Generic, Default)
deriveMapnikJSON ''Style
