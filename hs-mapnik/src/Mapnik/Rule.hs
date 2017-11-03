{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Rule where

import Mapnik.Imports
import Mapnik.Symbolizer
import Data.Text (Text)

newtype Expression = Expression {unExpression :: Text}
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Rule = Rule
  { _ruleName                    :: !(Maybe Text)
  , _ruleSymbolizers             :: ![Symbolizer]
  , _ruleFilter                  :: !(Maybe Expression)
  , _ruleMinimumScaleDenominator :: !(Maybe Double)
  , _ruleMaximumScaleDenominator :: !(Maybe Double)
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

makeClassy ''Rule
makeFields ''Rule
