{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.Parameter where

import           Mapnik.Imports

import           Data.Aeson
import           Data.Text (Text)
import qualified Data.HashMap.Strict as M
import           Data.String(IsString(..))
import           Data.Scientific (floatingOrInteger)


data ParamValue = StringParam !Text
                | DoubleParam !Double
                | IntParam    !Int
                | BoolParam   !Bool
                | NullParam
  deriving (Show, Eq, Generic)
makePrisms ''ParamValue

instance ToJSON ParamValue where
  toJSON (StringParam p) = toJSON p
  toJSON (DoubleParam p) = toJSON p
  toJSON (IntParam    p) = toJSON p
  toJSON (BoolParam   p) = toJSON p
  toJSON NullParam       = Null

instance FromJSON ParamValue where
  parseJSON (String s) = return (StringParam s)
  parseJSON (Number s) = case floatingOrInteger s of
                            Left v -> return (DoubleParam v)
                            Right v -> return (IntParam v)
  parseJSON (Bool s)   = return (BoolParam s)
  parseJSON Null       = return NullParam
  parseJSON _          = fail "parseJSON @ParamValue: Invalid type"

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
