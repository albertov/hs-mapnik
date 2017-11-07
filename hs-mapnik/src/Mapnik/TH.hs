{-# LANGUAGE FlexibleContexts #-}
module Mapnik.TH (deriveMapnikJSON) where


import Data.Char (toLower)
import Data.Aeson.Types
import Data.Aeson.TH

deriveMapnikJSON n = deriveJSON defaultOptions
  { sumEncoding = UntaggedValue
  , fieldLabelModifier = dropAndUncapitalize n
  , omitNothingFields = True
  } 

dropAndUncapitalize :: Int -> String -> String
dropAndUncapitalize n = uncapitalize . drop n

uncapitalize :: String -> String
uncapitalize s =
  case s of
    []     -> []
    (x:xs) -> toLower x : xs

