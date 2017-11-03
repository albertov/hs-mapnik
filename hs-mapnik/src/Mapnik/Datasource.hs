{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Datasource where

import Mapnik.Imports
import Mapnik.Parameter


newtype Datasource = Datasource Parameters
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
