{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Mapnik.TH (
    deriveMapnikJSON
  , makeMapnikFields
  , mapnikOptions
  ) where


import Data.Aeson.Types
import Data.Char (toUpper)
import Control.Lens
import Language.Haskell.TH

-- We don't use Aeson's TH From/ToJSONderivation because it
-- doesn't seem to respect omitNothingFields=True
deriveMapnikJSON :: Name -> DecsQ
deriveMapnikJSON name = [d|
  instance FromJSON $(return (ConT name)) where
    parseJSON = genericParseJSON mapnikOptions
  instance ToJSON $(return (ConT name)) where
    toJSON = genericToJSON mapnikOptions
    toEncoding = genericToEncoding mapnikOptions
  |]

mapnikOptions :: Options
mapnikOptions = defaultOptions
  { omitNothingFields = True
  , sumEncoding = TaggedObject "type" "value"
  }

makeMapnikFields :: Name -> DecsQ
makeMapnikFields = makeLensesWith $ defaultFieldRules
  & lensField .~ mapnikNameNamer

mapnikNameNamer :: FieldNamer
mapnikNameNamer _ _ field = [ MethodName (mkName cls) (mkName method) ]
  where
    fieldName = nameBase field
    method = fieldName
    cls = "Has" ++ capitalize fieldName

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x:xs
