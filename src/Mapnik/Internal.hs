{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Mapnik.Internal (
  Map (..)
, Image (..)
, Layer (..)
, Box (..)
, Datasource (..)
, Parameters (..)
, mapnikCtx
) where

import qualified Language.C.Inline.Cpp as C
import           Data.Monoid (mempty, (<>))
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import           Foreign.ForeignPtr (ForeignPtr)

newtype Map = Map (ForeignPtr Map)
newtype Image = Image (ForeignPtr Image)
newtype Layer = Layer (ForeignPtr Layer)
newtype Datasource = Datasource (ForeignPtr Datasource)
newtype Parameters = Parameters (ForeignPtr Parameters)

data Box = Box { x0, y0, x1, y1 :: {-# UNPACK #-}!Double }
  deriving (Eq, Show)

mapnikCtx :: Context
mapnikCtx = C.baseCtx <> C.cppCtx <> C.bsCtx <> C.fptrCtx <> ctx
  where ctx = mempty {
    ctxTypesTable =
      [ (C.TypeName "Map", [t| Map |])
      , (C.TypeName "image_rgba8", [t| Image |])
      , (C.TypeName "layer", [t| Layer |])
      , (C.TypeName "datasource_ptr", [t| Datasource |])
      , (C.TypeName "parameters", [t| Parameters |])
      ]
    }
