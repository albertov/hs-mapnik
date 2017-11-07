{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Expression where

import Mapnik.Imports
import Data.Text (Text)

newtype Expression = Expression {unExpression :: Text}
  deriving (Eq, Generic)
  deriving newtype (Show, ToJSON, FromJSON)
