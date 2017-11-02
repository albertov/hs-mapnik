{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Mapnik.Internal (
  Map (..)
, Image (..)
, mapnikCtx
) where

import qualified Language.C.Inline.Cpp as C
import           Data.Monoid (mempty, (<>))
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import           Foreign.ForeignPtr (ForeignPtr)

newtype Map = Map (ForeignPtr Map)
newtype Image = Image (ForeignPtr Image)

mapnikCtx :: Context
mapnikCtx = C.baseCtx <> C.cppCtx <> C.bsCtx <> C.fptrCtx <> ctx
  where ctx = mempty {
    ctxTypesTable =
      [ (C.TypeName "Map", [t| Map |])
      , (C.TypeName "image_rgba8", [t| Image |])
      ]
    }
