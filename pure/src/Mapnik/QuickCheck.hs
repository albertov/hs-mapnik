{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.QuickCheck where

import           Mapnik

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import           Data.Text.IO as T
import           System.IO.Unsafe (unsafePerformIO)
import           Test.QuickCheck hiding (label)
import           Test.QuickCheck.Instances ()
import           Prelude hiding (filter)
import           Paths_hs_mapnik (getDataFileName)

instance Arbitrary Map where
  arbitrary = resize 10 $ do
    backgroundColor        <- arbitrary
    backgroundImage        <- arbitrary
    backgroundImageCompOp  <- arbitrary
    backgroundImageOpacity <- arbitrary
    srs                    <- maybeArb arbitrarySrs
    bufferSize             <- arbitrary
    maximumExtent          <- arbitrary
    fontDirectory          <- arbitrary
    styles                 <- arbitrary
    layers                 <- listOf (arbitraryLayer (M.keys styles))
    pure Map{..}

instance Arbitrary Layer where
  arbitrary = arbitraryLayer =<< listOf arbitrary

instance Arbitrary Datasource where
  arbitrary = pure $ Datasource
    [ "type" .= ("memory" :: T.Text)
    ]

instance Arbitrary Style where
  arbitrary = do
    opacity             <- arbitrary
    imageFiltersInflate <- arbitrary
    rules               <- arbitrary
    pure Style{..}

instance Arbitrary Rule where
  arbitrary = do
    name                      <- maybeArb arbitrary
    symbolizers               <- resize 5 arbitrary
    filter                    <- arbitrary
    ( minimumScaleDenominator
     ,maximumScaleDenominator) <- arbitraryMinMaxScaleDenom
    pure Rule{..}

instance Arbitrary Expression where
  arbitrary = elements ["[some_field]"] --TODO

instance Arbitrary Transform where
  arbitrary = Transform <$> (T.intercalate " " <$> listOf1 parts)
    where
      parts = oneof
        [ pure "translate(4)"
        , pure "rotate(45, 50, 50)"
        ]



instance Arbitrary Color where
  arbitrary =
    RGBA <$> arbitrary <*> arbitrary
         <*> arbitrary <*> arbitrary

instance Arbitrary Box where
  arbitrary = do
    minx <- arbitrary
    miny <- arbitrary
    maxx <- (+minx) . getPositive <$> arbitrary
    maxy <- (+miny) . getPositive <$> arbitrary
    pure Box{..}

--------------------------------------------------------------------------------

instance Arbitrary Symbolizer where
  arbitrary = oneof
   [ arbitraryPointSym
   , arbitraryLineSym
   , arbitraryLinePatternSym
   , arbitraryPolygonSym
   , arbitraryPolygonPatternSym
   , arbitraryRasterSym
   , arbitraryShieldSym
   , arbitraryTextSym
   , arbitraryBuildingSym
   , arbitraryMarkersSym
   , arbitraryGroupSym
   , arbitraryDebugSym
   , arbitraryDotSym
   ]


arbitraryPointSym, arbitraryLineSym, arbitraryLinePatternSym, arbitraryPolygonSym, arbitraryPolygonPatternSym, arbitraryRasterSym :: Gen Symbolizer
arbitraryShieldSym, arbitraryTextSym, arbitraryBuildingSym, arbitraryMarkersSym, arbitraryGroupSym, arbitraryDebugSym, arbitraryDotSym :: Gen Symbolizer
arbitraryPointSym = do
  file            <- arbitrary
  opacity         <- arbitrary
  allowOverlap    <- arbitrary
  ignorePlacement <- arbitrary
  pointPlacement  <- arbitrary
  imageTransform  <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure PointSymbolizer{..}

arbitraryLineSym = do
  offset         <- arbitrary
  lineRasterizer <- arbitrary
  strokeGamma       <- arbitrary
  strokeGammaMethod <- arbitrary
  strokeDashArray   <- arbitrary
  strokeDashOffset  <- arbitrary
  strokeMiterLimit  <- arbitrary
  strokeWidth       <- arbitrary
  strokeOpacity     <- arbitrary
  stroke            <- arbitrary
  strokeLineJoin    <- arbitrary
  strokeLineCap     <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure LineSymbolizer{..}

arbitraryLinePatternSym = do
  file             <- arbitrary
  opacity          <- arbitrary
  offset           <- arbitrary
  imageTransform   <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure LinePatternSymbolizer{..}

arbitraryPolygonSym = do
  fill         <- arbitrary
  fillOpacity  <- arbitrary
  gamma        <- arbitrary
  gammaMethod  <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure PolygonSymbolizer{..}

arbitraryPolygonPatternSym = do
  file            <- arbitrary
  opacity         <- arbitrary
  gamma           <- arbitrary
  gammaMethod     <- arbitrary
  imageTransform  <- arbitrary
  alignment       <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure PolygonPatternSymbolizer{..}

arbitraryRasterSym = do
  scaling       <- arbitrary
  rasterOpacity <- arbitrary
  filterFactor  <- arbitrary
  meshSize      <- arbitrary
  preMultiplied <- arbitrary
  colorizer     <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure RasterSymbolizer{..}

arbitraryShieldSym = do
  placements      <- arbitrary
  imageTransform  <- arbitrary
  dx              <- arbitrary
  dy              <- arbitrary
  opacity         <- arbitrary
  unlockImage     <- arbitrary
  file            <- arbitrary
  haloRasterizer  <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure ShieldSymbolizer{..}
 
arbitraryTextSym = do
  placements     <- arbitrary
  haloCompOp     <- arbitrary
  haloRasterizer <- arbitrary
  haloTransform  <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure TextSymbolizer{..}
  
arbitraryBuildingSym = do
  fill        <- arbitrary
  fillOpacity <- arbitrary
  height      <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure BuildingSymbolizer{..}

arbitraryMarkersSym = do
  file            <- arbitrary
  opacity         <- arbitrary
  fill            <- arbitrary
  fillOpacity     <- arbitrary
  spacing         <- arbitrary
  maxError        <- arbitrary
  offset          <- arbitrary
  width           <- arbitrary
  height          <- arbitrary
  allowOverlap    <- arbitrary
  avoidEdges      <- arbitrary
  ignorePlacement <- arbitrary
  imageTransform  <- arbitrary
  placement       <- arbitrary
  multiPolicy     <- arbitrary
  direction       <- arbitrary
  strokeGamma       <- arbitrary
  strokeGammaMethod <- arbitrary
  strokeDashArray   <- arbitrary
  strokeDashOffset  <- arbitrary
  strokeMiterLimit  <- arbitrary
  strokeWidth       <- arbitrary
  strokeOpacity     <- arbitrary
  stroke            <- arbitrary
  strokeLineJoin    <- arbitrary
  strokeLineCap     <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure MarkersSymbolizer{..}

arbitraryGroupSym = do
  groupProperties <- arbitrary
  numColumns      <- arbitrary
  startColumn     <- arbitrary
  repeatKey       <- arbitrary
  placements      <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure GroupSymbolizer{..}

arbitraryDebugSym = do
  mode <- arbitrary
  simplifyTolerance <- arbitrary
  smooth            <- arbitrary
  clip              <- arbitrary
  compOp            <- arbitrary
  geometryTransform <- arbitrary
  simplifyAlgorithm <- arbitrary
  pure DebugSymbolizer{..}

arbitraryDotSym = do
  fill    <- arbitrary
  opacity <- arbitrary
  width   <- arbitrary
  height  <- arbitrary
  compOp  <- arbitrary
  pure DotSymbolizer{..}



instance Arbitrary CompositeMode where arbitrary = arbitraryEnum
instance Arbitrary PointPlacement where arbitrary = arbitraryEnum
instance Arbitrary LineRasterizer where arbitrary = arbitraryEnum
instance Arbitrary PatternAlignment where arbitrary = arbitraryEnum
instance Arbitrary ScalingMethod where arbitrary = arbitraryEnum
instance Arbitrary HaloRasterizer where arbitrary = arbitraryEnum
instance Arbitrary MarkerPlacement where arbitrary = arbitraryEnum
instance Arbitrary MarkerMultiPolicy where arbitrary = arbitraryEnum
instance Arbitrary Direction where arbitrary = arbitraryEnum
instance Arbitrary GammaMethod where arbitrary = arbitraryEnum
instance Arbitrary LineJoin where arbitrary = arbitraryEnum
instance Arbitrary LineCap where arbitrary = arbitraryEnum
instance Arbitrary DebugMode where arbitrary = arbitraryEnum
instance Arbitrary ColorizerMode where arbitrary = arbitraryEnum
instance Arbitrary SimplifyAlgorithm where arbitrary = arbitraryEnum
instance Arbitrary LabelPlacement where arbitrary = arbitraryEnum
instance Arbitrary Upright where arbitrary = arbitraryEnum
instance Arbitrary HorizontalAlignment where arbitrary = arbitraryEnum
instance Arbitrary JustifyAlignment where arbitrary = arbitraryEnum
instance Arbitrary VerticalAlignment where arbitrary = arbitraryEnum
instance Arbitrary PlacementDirection where arbitrary = arbitraryEnum
instance Arbitrary TextTransform where arbitrary = arbitraryEnum
instance Arbitrary AspectFixMode where arbitrary = arbitraryEnum

instance Arbitrary a => Arbitrary (Prop a) where
  arbitrary = oneof [ Exp <$> arbitrary, Val <$> arbitrary ]

instance Arbitrary Dash where
  arbitrary = Dash <$> (getPositive <$> arbitrary)
                   <*> (getPositive <$> arbitrary)

instance Arbitrary Value where
  arbitrary = oneof [ TextValue   <$> arbitrary
                    , DoubleValue <$> arbitrary
                    , IntValue    <$> arbitrary
                    , BoolValue   <$> arbitrary
                    , pure NullValue
                    ]

instance Arbitrary Colorizer where
  arbitrary = do
    mode   <- arbitrary
    color  <- arbitrary
    stops  <- arbitrary
    pure Colorizer{..}

instance Arbitrary Stop where
  arbitrary = do
    value <- arbitrary
    color <- arbitrary
    mode  <- arbitrary
    label <- maybeArb arbitrary
    pure Stop{..}

instance Arbitrary GroupProperties where
  arbitrary = pure GroupProperties --TODO

instance Arbitrary TextPlacements where
  arbitrary = Dummy <$> arbitrary

instance Arbitrary TextSymProperties where
  arbitrary = do
    properties       <- arbitrary
    layoutProperties <- arbitrary
    formatProperties <- arbitrary
    format           <- arbitrary
    pure TextSymProperties{..}

instance Arbitrary TextProperties where
  arbitrary = do
    labelPlacement          <- arbitrary
    labelSpacing            <- arbitrary
    labelPositionTolerance  <- arbitrary
    avoidEdges              <- arbitrary
    margin                  <- arbitrary
    repeatDistance          <- arbitrary
    minimumDistance         <- arbitrary
    minimumPadding          <- arbitrary
    minimumPathLength       <- arbitrary
    maxCharAngleDelta       <- arbitrary
    allowOverlap            <- arbitrary
    largestBoxOnly          <- arbitrary
    upright                 <- arbitrary
    pure TextProperties{..}

instance Arbitrary TextLayoutProperties where
  arbitrary = do
    dx                  <- arbitrary
    dy                  <- arbitrary
    orientation         <- arbitrary
    textRatio           <- arbitrary
    wrapWidth           <- arbitrary
    wrapChar            <- arbitrary
    wrapBefore          <- arbitrary
    repeatWrapChar      <- arbitrary
    rotateDisplacement  <- arbitrary
    horizontalAlignment <- arbitrary
    justifyAlignment    <- arbitrary
    verticalAlignment   <- arbitrary
    direction           <- arbitrary
    pure TextLayoutProperties{..}

instance Arbitrary TextFormatProperties where
  arbitrary = do
    faceName         <- maybeArb arbitraryFaceName
    fontSet          <- arbitrary
    textSize         <- arbitrary
    characterSpacing <- arbitrary
    lineSpacing      <- arbitrary
    textOpacity      <- arbitrary
    haloOpacity      <- arbitrary
    textTransform    <- arbitrary
    fill             <- arbitrary
    haloFill         <- arbitrary
    haloRadius       <- arbitrary
    ffSettings       <- arbitrary
    pure TextFormatProperties{..}

instance Arbitrary Format where
  --TODO implement shrink
  arbitrary = oneof
    [ sized arbitraryFormat
    , arbitraryFormatList
    , arbitraryFormatExp
    , sized arbitraryFormatLayout
    , pure NullFormat
    ]

arbitraryFormat :: Int -> Gen Format
arbitraryFormat 0 = pure NullFormat
arbitraryFormat n = do
  faceName         <- maybeArb arbitraryFaceName
  fontSet          <- arbitrary
  textSize         <- arbitrary
  characterSpacing <- arbitrary
  lineSpacing      <- arbitrary
  wrapBefore       <- arbitrary
  repeatWrapChar   <- arbitrary
  textTransform    <- arbitrary
  fill             <- arbitrary
  haloFill         <- arbitrary
  haloRadius       <- arbitrary
  ffSettings       <- arbitrary
  next             <- resize (n-1) arbitrary
  pure Format{..}

arbitraryFormatLayout :: Int -> Gen Format
arbitraryFormatLayout 0 = pure NullFormat
arbitraryFormatLayout n = do
  dx                  <- arbitrary
  dy                  <- arbitrary
  orientation         <- arbitrary
  textRatio           <- arbitrary
  wrapWidth           <- arbitrary
  wrapChar            <- arbitrary
  wrapBefore          <- arbitrary
  repeatWrapChar      <- arbitrary
  rotateDisplacement  <- arbitrary
  horizontalAlignment <- arbitrary
  justifyAlignment    <- arbitrary
  verticalAlignment   <- arbitrary
  next                <- resize n arbitrary
  pure FormatLayout{..}

arbitraryFormatList :: Gen Format
arbitraryFormatList = FormatList <$> listOf (oneof
    [ sized arbitraryFormat
    , arbitraryFormatExp
    , sized arbitraryFormatLayout
    ])

arbitraryFormatExp :: Gen Format
arbitraryFormatExp = FormatExp <$> arbitrary

instance Arbitrary FontSet where
  arbitrary = pure FontSet --TODO

instance Arbitrary FontFeatureSettings where
  arbitrary = FontFeatureSettings <$> parts
    where
      parts = oneof
        [ pure "smcp"
        , pure "hist"
        , pure "frac"
        ]


-----------------------------------------------------------------------------
arbitraryEnum :: (Enum a, Bounded a) => Gen a
arbitraryEnum = elements [minBound..maxBound]

arbitraryFaceName :: Gen FaceName
arbitraryFaceName = elements
  [ "DejaVu Sans Book"
  ]

arbitrarySrs :: Gen Proj4
arbitrarySrs = elements allSrs

allSrs :: [Proj4]
allSrs = unsafePerformIO (T.lines <$> (T.readFile =<< getDataFileName "spec/proj4s.txt"))
{-# NOINLINE allSrs #-}

arbitraryMinMaxScaleDenom :: Gen (Maybe Double, Maybe Double)
arbitraryMinMaxScaleDenom = do
  min_ <- maybeArb (getPositive <$> arbitrary)
  max_ <- case min_ of
    Just min' -> maybeArb ((+min') . getNonNegative <$> arbitrary)
    Nothing   -> maybeArb (getPositive <$> arbitrary)
  pure (min_, max_)

arbitraryLayer :: [StyleName] -> Gen Layer
arbitraryLayer stylesInMap = do
  name                    <- arbitrary
  dataSource              <- arbitrary
  styles                  <- if null stylesInMap then pure [] else
                             listOf (elements stylesInMap)
  srs                     <- maybeArb arbitrarySrs
  ( minimumScaleDenominator
   ,maximumScaleDenominator) <- arbitraryMinMaxScaleDenom
  queryable               <- arbitrary
  clearLabelCache         <- arbitrary
  cacheFeatures           <- arbitrary
  groupBy                 <- maybeArb arbitrary
  bufferSize              <- maybeArb (getNonNegative <$> arbitrary)
  maximumExtent           <- arbitrary
  pure Layer{..}

maybeArb :: Gen a -> Gen (Maybe a)
maybeArb gen = oneof [pure Nothing, Just <$> gen]
