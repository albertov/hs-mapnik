{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Symbolizer where

import Mapnik.Imports
import Data.Text

data Symbolizer = Symbolizer
  { 
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

makeClassy ''Symbolizer
makeFields ''Symbolizer
