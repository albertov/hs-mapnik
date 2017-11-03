{-# LANGUAGE FlexibleContexts #-}
module Mapnik.Util where


import Data.Char (toLower)
import Data.Aeson.Types

defaultMapnikOptions :: Options
defaultMapnikOptions = defaultOptions {
    omitNothingFields = True
  , sumEncoding = ObjectWithSingleField
  }

mapnikJsonDecoder n = genericParseJSON defaultMapnikOptions {
    fieldLabelModifier = dropAndUncapitalize n
  }
mapnikJsonEncoder n = genericToEncoding defaultMapnikOptions {
    fieldLabelModifier = dropAndUncapitalize n
  }

dropAndUncapitalize n s =
  case drop n s of
    []     -> []
    (x:xs) -> toLower x : xs

