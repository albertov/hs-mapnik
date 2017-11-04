{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Style where

import Mapnik.Imports
import Mapnik.Rule
import Data.Text
import qualified Data.HashMap.Strict as M

type StyleName = Text

type Styles = M.HashMap StyleName Style

data Style = Style
  { _styleOpacity             :: !(Maybe Double)
  , _styleImageFiltersInflate :: !(Maybe Bool)
  , _styleRules               :: ![Rule]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

makeClassy ''Style
makeFields ''Style
