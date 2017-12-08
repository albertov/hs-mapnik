{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Rule where

import Mapnik.Imports
import Mapnik.Common
import Mapnik.Symbolizer
import Data.Default (Default)
import Data.Text (Text)

import Prelude hiding (length)

data Rule = Rule
  { name                    :: !(Maybe Text)
  , symbolizers             :: ![Symbolizer]
  , filter                  :: !(Maybe Expression)
  , hasElse                 :: !(Maybe Bool)
  , hasAlso                 :: !(Maybe Bool)
  , minimumScaleDenominator :: !(Maybe Double)
  , maximumScaleDenominator :: !(Maybe Double)
  } deriving (Eq, Show, Generic, Default)
deriveMapnikJSON ''Rule
