{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Mapnik.Datasource where

import Mapnik.Imports
import Mapnik.Parameter


newtype Datasource = Datasource Parameters
  deriving (Generic)
  deriving newtype (Eq, Show, ToJSON, FromJSON)
