{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Mapnik.Purescript (generatePS) where

import           Mapnik

import           Data.Proxy
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.TypeParameters (A)

--
-- Purescript bridge
--

generatePS :: FilePath -> IO ()
generatePS frontEndRoot = writePSTypes frontEndRoot (buildBridge myBridge) myTypes


myBridge :: BridgePart
myBridge = defaultBridge


myTypes :: [SumType 'Haskell]
myTypes = [ mkSumType (Proxy :: Proxy Map)
          , mkSumType (Proxy :: Proxy Layer)
          , mkSumType (Proxy :: Proxy Style)
          , mkSumType (Proxy :: Proxy Rule)
          , mkSumType (Proxy :: Proxy Symbolizer)
          , mkSumType (Proxy :: Proxy Expression)
          , mkSumType (Proxy :: Proxy Color)
          , mkSumType (Proxy :: Proxy Transform)
          , mkSumType (Proxy :: Proxy Box)
          , mkSumType (Proxy :: Proxy Dash)
          --, mkSumType (Proxy :: Proxy DashArray)
          --, mkSumType (Proxy :: Proxy Proj4)
          , mkSumType (Proxy :: Proxy (Prop A))

          -- Enums
          , mkSumType (Proxy :: Proxy CompositeMode)
          , mkSumType (Proxy :: Proxy AspectFixMode)
          , mkSumType (Proxy :: Proxy LineCap)
          , mkSumType (Proxy :: Proxy LineJoin)
          , mkSumType (Proxy :: Proxy LineRasterizer)
          , mkSumType (Proxy :: Proxy HaloRasterizer)
          , mkSumType (Proxy :: Proxy PointPlacement)
          , mkSumType (Proxy :: Proxy PatternAlignment)
          , mkSumType (Proxy :: Proxy DebugMode)
          , mkSumType (Proxy :: Proxy MarkerPlacement)
          , mkSumType (Proxy :: Proxy MarkerMultiPolicy)
          , mkSumType (Proxy :: Proxy TextTransform)
          , mkSumType (Proxy :: Proxy LabelPlacement)
          , mkSumType (Proxy :: Proxy VerticalAlignment)
          , mkSumType (Proxy :: Proxy HorizontalAlignment)
          , mkSumType (Proxy :: Proxy JustifyAlignment)
          , mkSumType (Proxy :: Proxy Upright)
          , mkSumType (Proxy :: Proxy Direction)
          , mkSumType (Proxy :: Proxy PlacementDirection)
          , mkSumType (Proxy :: Proxy GammaMethod)
          , mkSumType (Proxy :: Proxy RasterMode)
          ]
