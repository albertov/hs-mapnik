{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Rule where

import Mapnik.Imports
import Mapnik.Common
import Mapnik.Symbolizer
import Data.Text (Text)


data Rule = Rule
  { _ruleName                    :: !(Maybe Text)
  , _ruleSymbolizers             :: ![Symbolizer]
  , _ruleFilter                  :: !(Maybe Expression)
  , _ruleMinimumScaleDenominator :: !(Maybe Double)
  , _ruleMaximumScaleDenominator :: !(Maybe Double)
  } deriving (Eq, Show, Generic)
deriveMapnikJSON 5 ''Rule
makeClassy ''Rule
makeFields ''Rule
