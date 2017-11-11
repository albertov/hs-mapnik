{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Parameter where

import           Mapnik.Imports

import           Data.Text (Text)
import           Data.String(IsString(..))
import qualified Data.HashMap.Strict as M


data ParamValue = StringParam !Text
                | DoubleParam !Double
                | IntParam    !Int
                | BoolParam   !Bool
                | NullParam
  deriving (Show, Eq, Generic)
deriveMapnikJSON ''ParamValue

class ToParam p where
  toParam :: p -> ParamValue
instance ToParam ParamValue where toParam = id
instance ToParam String where toParam = StringParam . fromString
instance ToParam Text where toParam = StringParam
instance ToParam Double where toParam = DoubleParam
instance ToParam Int where toParam = IntParam
instance ToParam Bool where toParam = BoolParam
instance ToParam a => ToParam (Maybe a) where
  toParam (Just a) = toParam a
  toParam Nothing  = NullParam

type Parameter = (Text, ParamValue)

(.=) :: ToParam v => String -> v -> Parameter
k .= v = (fromString k, toParam v)

instance IsString ParamValue where
  fromString = StringParam . fromString

type Parameters = M.HashMap Text ParamValue
