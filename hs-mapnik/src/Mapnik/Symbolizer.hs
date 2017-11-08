{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Mapnik.Symbolizer (
  module Mapnik.Symbolizer
, module Mapnik.Symbolizer.Property
, module Mapnik.Symbolizer.TextProperties
, DSum (..)
) where

import Mapnik.Imports
import Mapnik.Symbolizer.TextProperties
import Mapnik.Symbolizer.Property

import Data.Aeson
import Data.Text (Text)
import qualified Data.Dependent.Map as DMap
import qualified Data.HashMap.Strict as HM



data Symbolizer
  = Point          { properties :: Properties }
  | Line           { properties :: Properties }
  | LinePattern    { properties :: Properties }
  | Polygon        { properties :: Properties }
  | PolygonPattern { properties :: Properties }
  | Raster         { properties :: Properties }
  | Shield         { properties :: Properties }
  | Text           { properties :: Properties }
  | Building       { properties :: Properties }
  | Marker         { properties :: Properties }
  | Group          { properties :: Properties }
  | Debug          { properties :: Properties }
  | Dot            { properties :: Properties }
  deriving (Eq, Show, Generic)

symType :: Symbolizer -> Text
symType Point{} = "point"
symType Line{} = "line"
symType LinePattern{} = "linePattern"
symType Polygon{} = "polygon"
symType PolygonPattern{} = "polygonPattern"
symType Raster{} = "raster"
symType Shield{} = "shield"
symType Text{} = "text"
symType Building{} = "building"
symType Marker{} = "marker"
symType Group{} = "group"
symType Debug{} = "debug"
symType Dot{} = "dot"

toProperties :: Symbolizer -> [DSum Key PropValue]
toProperties = DMap.toList . properties

instance ToJSON Symbolizer where
  toJSON sym = object (("type",toJSON (symType sym)):props)
    where props = map serializeProperty  (toProperties sym)

instance FromJSON Symbolizer where
  parseJSON = withObject "Symbolizer" $ \o -> do
    type_ :: Text <- o .: "type" 
    props <- parseProperties (HM.delete "type" o)
    case type_ of
      "point" -> return (Point props)
      "line" -> return (Line props)
      "linePattern" -> return (LinePattern props)
      "polygon" -> return (Polygon props)
      "polygonPattern" -> return (PolygonPattern props)
      "raster" -> return (Raster props)
      "shield" -> return (Shield props)
      "text" -> return (Text props)
      "building" -> return (Building props)
      "marker" -> return (Marker props)
      "group" -> return (Group props)
      "debug" -> return (Debug props)
      "dot" -> return (Dot props)
      _     -> fail "Unknown symbolizer type"
