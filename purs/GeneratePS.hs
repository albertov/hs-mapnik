{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NamedFieldPuns        #-}

import           System.Environment (getArgs)

import           Mapnik

import           Control.Lens
import           Data.Text (Text)
import           Data.Proxy
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeParameters (A)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [outputDir] -> generatePS outputDir
    _ -> putStrLn "Usage: hs-mapnik-purs <outputDir>"

--
-- Purescript bridge
--

generatePS :: FilePath -> IO ()
generatePS frontEndRoot = writePSTypes frontEndRoot (buildBridge myBridge) myTypes


myBridge :: BridgePart
myBridge = defaultBridge
       <|> vectorBridge
       <|> wordBridge
       <|> floatBridge
       <|> charBridge
       <|> stringMapBridge

stringMapBridge :: BridgePart
stringMapBridge = do
  typeName ^== "HashMap"
  doCheck typeParameters isStringMap
  [_,x] <- psTypeParameters
  return TypeInfo
    { _typePackage = "purescript-maps"
    , _typeModule = "Data.StrMap"
    , _typeName = "StrMap"
    , _typeParameters = [x]
    }

isStringMap :: [TypeInfo lang] -> Bool
isStringMap [x,_] = (x^.typeName) `elem` (["Text","String"] :: [Text])
isStringMap _     = False

vectorBridge :: BridgePart
vectorBridge = do
  typeName ^== "Vector"
  ps <- psTypeParameters
  return TypeInfo
    { _typePackage = "purescript-prelude"
    , _typeModule = "Prim"
    , _typeName = "Array"
    , _typeParameters = ps
    }

wordBridge :: BridgePart
wordBridge = do
  typeName ^== "Word8"
  return psInt

charBridge :: BridgePart
charBridge = do
  typeName ^== "Char"
  return psString

floatBridge :: BridgePart
floatBridge = do
  typeName ^== "Float"
  return psNumber

myTypes :: [SumType 'Haskell]
myTypes = [ mkSumType (Proxy :: Proxy Map)
          , mkSumType (Proxy :: Proxy Layer)
          , mkSumType (Proxy :: Proxy Style)
          , mkSumType (Proxy :: Proxy Rule)
          , mkSumType (Proxy :: Proxy Symbolizer)
          , mkSumType (Proxy :: Proxy Expression)
          , mkSumType (Proxy :: Proxy PathExpression)
          , mkSumType (Proxy :: Proxy Color)
          , mkSumType (Proxy :: Proxy Transform)
          , mkSumType (Proxy :: Proxy Box)
          , mkSumType (Proxy :: Proxy Dash)
          , mkSumType (Proxy :: Proxy Datasource)
          , mkSumType (Proxy :: Proxy Value)
          , mkSumType (Proxy :: Proxy (Prop A))
          , mkSumType (Proxy :: Proxy FontFeatureSettings)
          , mkSumType (Proxy :: Proxy Stop)
          , mkSumType (Proxy :: Proxy Colorizer)
          , mkSumType (Proxy :: Proxy TextProperties)
          , mkSumType (Proxy :: Proxy TextSymProperties)
          , mkSumType (Proxy :: Proxy GroupSymProperties)
          , mkSumType (Proxy :: Proxy GroupRule)
          , mkSumType (Proxy :: Proxy GroupLayout)
          , mkSumType (Proxy :: Proxy TextLayoutProperties)
          , mkSumType (Proxy :: Proxy TextFormatProperties)
          , mkSumType (Proxy :: Proxy Format)
          , mkSumType (Proxy :: Proxy TextPlacements)
          , mkSumType (Proxy :: Proxy Font)
          , mkSumType (Proxy :: Proxy ImageFilter)
          , mkSumType (Proxy :: Proxy ColorStop)

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
          , mkSumType (Proxy :: Proxy ColorizerMode)
          , mkSumType (Proxy :: Proxy ScalingMethod)
          , mkSumType (Proxy :: Proxy SimplifyAlgorithm)
          , mkSumType (Proxy :: Proxy FilterMode)
          ]
