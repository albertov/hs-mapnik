{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
module Mapnik.Bindings (
  Map (..)
, Image (..)
, Layer (..)
, Style (..)
, Rule (..)
, Symbolizer (..)
, SymbolizerBase (..)
, Expression (..)
, Box (..)
, Datasource (..)
, Parameters (..)
, Projection (..)
, ProjTransform (..)
, Color (..)
, C.CppException (..)
, mapnikCtx
) where

import           Mapnik (Box(..))

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import           Data.Monoid (mempty, (<>))
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import           Foreign.ForeignPtr (ForeignPtr)


newtype Map = Map (ForeignPtr Map)
  deriving (Eq, Show)
newtype Image = Image (ForeignPtr Image)
  deriving (Eq, Show)
newtype Layer = Layer (ForeignPtr Layer)
  deriving (Eq, Show)
newtype Datasource = Datasource (ForeignPtr Datasource)
  deriving (Eq, Show)
newtype Parameters = Parameters (ForeignPtr Parameters)
  deriving (Eq, Show)
newtype Projection = Projection (ForeignPtr Projection)
  deriving (Eq)
newtype ProjTransform = ProjTransform (ForeignPtr ProjTransform)
  deriving (Eq, Show)
newtype Color = Color (ForeignPtr Color)
  deriving (Eq)
newtype Style = Style (ForeignPtr Style)
  deriving (Eq, Show)
newtype Rule = Rule (ForeignPtr Rule)
  deriving (Eq, Show)
newtype Symbolizer = Symbolizer (ForeignPtr Symbolizer)
  deriving (Eq, Show)
newtype SymbolizerBase = SymbolizerBase (ForeignPtr SymbolizerBase)
  deriving (Eq, Show)
newtype Expression = Expression (ForeignPtr Expression)
  deriving (Eq)

mapnikCtx :: Context
mapnikCtx = C.baseCtx <> C.cppCtx <> C.bsCtx <> C.fptrCtx <> C.funCtx <> ctx
  where ctx = mempty {
    ctxTypesTable =
      [ (C.TypeName "Map", [t| Map |])
      , (C.TypeName "image_rgba8", [t| Image |])
      , (C.TypeName "layer", [t| Layer |])
      , (C.TypeName "datasource_ptr", [t| Datasource |])
      , (C.TypeName "parameters", [t| Parameters |])
      , (C.TypeName "projection", [t| Projection |])
      , (C.TypeName "proj_transform", [t| ProjTransform |])
      , (C.TypeName "color", [t| Color |])
      , (C.TypeName "feature_type_style", [t| Style |])
      , (C.TypeName "rule", [t| Rule |])
      , (C.TypeName "symbolizer", [t| Symbolizer |])
      , (C.TypeName "symbolizer_base", [t| SymbolizerBase |])
      , (C.TypeName "expression_ptr", [t| Expression |])
      , (C.TypeName "keys", [t| C.CUChar |])
      ]
    }
