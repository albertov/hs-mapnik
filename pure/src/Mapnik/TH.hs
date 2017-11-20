{-# LANGUAGE FlexibleContexts #-}
module Mapnik.TH (
    deriveMapnikJSON
  , makeMapnikFields
  , mapnikOptions
  ) where


import Data.Aeson.Types
import Data.Aeson.TH
import Data.Char (toUpper)
import Control.Lens
import Language.Haskell.TH

deriveMapnikJSON :: Name -> DecsQ
deriveMapnikJSON = deriveJSON mapnikOptions

mapnikOptions :: Options
mapnikOptions = defaultOptions
  { 
    sumEncoding = TaggedObject "type" "value"
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
