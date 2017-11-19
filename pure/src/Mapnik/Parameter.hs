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


data Value = TextValue !Text
           | DoubleValue !Double
           | IntValue    !Int
           | BoolValue   !Bool
           | NullValue
  deriving (Show, Eq, Generic)
deriveMapnikJSON ''Value

class ToValue p where
  toValue :: p -> Value
instance ToValue Value where toValue = id
instance ToValue String where toValue = TextValue . fromString
instance ToValue Text where toValue = TextValue
instance ToValue Double where toValue = DoubleValue
instance ToValue Int where toValue = IntValue
instance ToValue Bool where toValue = BoolValue
instance ToValue a => ToValue (Maybe a) where
  toValue (Just a) = toValue a
  toValue Nothing  = NullValue

type Parameter = (Text, Value)

(.=) :: ToValue v => String -> v -> Parameter
k .= v = (fromString k, toValue v)

instance IsString Value where
  fromString = TextValue . fromString

type Parameters = M.HashMap Text Value
