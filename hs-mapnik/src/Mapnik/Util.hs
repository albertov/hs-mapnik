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

dropAndUncapitalize :: Int -> String -> String
dropAndUncapitalize n = uncapitalize . drop n

uncapitalize :: String -> String
uncapitalize s =
  case s of
    []     -> []
    (x:xs) -> toLower x : xs

