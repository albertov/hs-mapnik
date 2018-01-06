{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Mapnik.QuickCheck where

import           Mapnik.Map
import           Mapnik.Common
import           Mapnik.Layer
import           Mapnik.Datasource
import           Mapnik.Style
import           Mapnik.Rule
import           Mapnik.Symbolizer
import           Mapnik.Color
import           Mapnik.ImageFilter
import           Mapnik.Enums
import           Mapnik.Parameter

import           Data.Char (isPrint)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector.Storable as St
import           Data.Text.IO as T
import           System.IO.Unsafe (unsafePerformIO)
import           Test.QuickCheck hiding (label)
import           Prelude hiding (filter)
import qualified Prelude as P
import           Paths_hs_mapnik (getDataFileName)

instance Arbitrary s => Arbitrary (Map s) where
  arbitrary = resize 10 $ do
    backgroundColor        <- arbitrary
    backgroundImage        <- arbitrary
    backgroundImageCompOp  <- arbitrary
    backgroundImageOpacity <- arbitrary
    srs                    <- maybeArb arbitrarySrs
    bufferSize             <- arbitrary
    maximumExtent          <- arbitrary
    fontDirectory          <- arbitrary
    basePath               <- arbitrary
    fontSets               <- arbitrary
    parameters             <- arbitrary
    styles                 <- M.fromList <$> listOf ((,) <$> arbitrary <*> arbitraryStyle fontSets)
    layers                 <- listOf (arbitraryLayer (M.keys styles))
    pure Map{..}

instance Arbitrary s => Arbitrary (Layer s) where
  arbitrary = arbitraryLayer =<< listOf arbitrary

instance Arbitrary Datasource where
  arbitrary = pure $ Datasource
    [ "type" .= ("memory" :: T.Text)
    ]

arbitraryStyle :: FontSetMap -> Gen Style
arbitraryStyle fontMap = do
  opacity             <- arbitrary
  imageFiltersInflate <- arbitrary
  filters             <- scale (min 5) arbitrary
  directFilters       <- scale (min 5) arbitrary
  filterMode          <- arbitrary
  compOp              <- arbitrary
  rules               <- listOf (arbitraryRule fontMap)
  pure Style{..}

arbitraryRule :: FontSetMap -> Gen Rule
arbitraryRule fontMap = do
  name                      <- maybeArb arbitrary
  symbolizers               <- scale (min 5) (listOf (arbitrarySymbolizer fontMap))
  filter                    <- arbitrary
  hasElse                   <- arbitrary
  hasAlso                   <- arbitrary
  ( minimumScaleDenominator
   ,maximumScaleDenominator) <- arbitraryMinMaxScaleDenom
  pure Rule{..}

instance Arbitrary Expression where
  arbitrary = elements ["[GEONAME]", "[SCALE_CAT]"] --TODO

instance Arbitrary PathExpression where
  arbitrary = elements ["/foo/[BAR].gif", "[SCALE_CAT]"] --TODO


instance Arbitrary Transform where
  arbitrary = Transform <$> (T.intercalate " " <$> listOf1 parts)
    where
      parts = oneof
        [ pure "translate(4)"
        , pure "rotate(45, 50, 50)"
        ]

instance Arbitrary PixelRgba8 where
  arbitrary = PixelRgba8 <$> arbitrary

instance Arbitrary ImageRgba8 where
  arbitrary = do
    w <- getPositive <$> arbitrary
    h <- getPositive <$> arbitrary
    pxs <- St.fromList <$> vectorOf (w*h) arbitrary
    pure (ImageRgba8 (w,h) pxs)

instance Arbitrary ImageFilter where
  arbitrary = oneof
        [ pure Blur
        , pure Emboss
        , pure Sharpen
        , pure EdgeDetect
        , pure Sobel
        , pure Gray
        , pure XGradient
        , pure YGradient
        , pure Invert
        , pure ColorBlindProtanope
        , pure ColorBlindDeuteranope
        , pure ColorBlindTritanope
        , AggStackBlur <$> arbitrary <*> arbitrary
        , ColorToAlpha <$> arbitrary
        , ScaleHsla <$> arbitrary <*> arbitrary
                    <*> arbitrary <*> arbitrary
                    <*> arbitrary <*> arbitrary
                    <*> arbitrary <*> arbitrary
        , ColorizeAlpha <$> scale (min 10) arbitrary
        ]

instance Arbitrary ColorStop where
  arbitrary = ColorStop <$> arbitrary <*> arbitrary

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

arbitrarySymbolizer :: FontSetMap -> Gen Symbolizer
arbitrarySymbolizer fontMap = oneof
   [ arbitraryPointSym
   , arbitraryLineSym
   , arbitraryLinePatternSym
   , arbitraryPolygonSym
   , arbitraryPolygonPatternSym
   , arbitraryRasterSym
   , arbitraryShieldSym fontMap
   , arbitraryTextSym fontMap
   , arbitraryBuildingSym
   , arbitraryMarkersSym
   , arbitraryGroupSym fontMap
   , arbitraryDebugSym
   , arbitraryDotSym
   ]


arbitraryPointSym, arbitraryLineSym, arbitraryLinePatternSym, arbitraryPolygonSym, arbitraryPolygonPatternSym, arbitraryRasterSym :: Gen Symbolizer
arbitraryBuildingSym, arbitraryMarkersSym, arbitraryDebugSym, arbitraryDotSym :: Gen Symbolizer
arbitraryTextSym, arbitraryGroupSym, arbitraryShieldSym :: FontSetMap -> Gen Symbolizer

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
  strokeDashArray   <- maybeArb arbitraryStrokeDashArrayProp
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

arbitraryShieldSym fontMap = do
  placements      <- arbitraryPlacements fontMap
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

arbitraryTextSym fontMap = do
  placements     <- arbitraryPlacements fontMap
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
  strokeDashArray   <- maybeArb arbitraryStrokeDashArrayProp
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

arbitraryStrokeDashArrayProp :: Gen (Prop (St.Vector Dash))
arbitraryStrokeDashArrayProp =
  arbitraryPropWith (St.fromList <$> arbitrary)

arbitraryGroupSym fontMap = do
  groupProperties <- arbitraryGroupSymProperties fontMap
  numColumns      <- arbitrary
  startColumn     <- arbitrary
  repeatKey       <- arbitrary
  placements      <- arbitraryPlacements fontMap
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
instance Arbitrary FilterMode where arbitrary = arbitraryEnum

instance Arbitrary a => Arbitrary (Prop a) where
  arbitrary = arbitraryProp

arbitraryProp :: Arbitrary a => Gen (Prop a)
arbitraryProp = arbitraryPropWith arbitrary

arbitraryPropWith :: Gen a -> Gen (Prop a)
arbitraryPropWith a = oneof [ Exp <$> arbitrary, Val <$> a ]

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
    epsilon  <- arbitrary
    pure Colorizer{..}

instance Arbitrary Stop where
  arbitrary = do
    value <- arbitrary
    color <- arbitrary
    mode  <- arbitrary
    label <- maybeArb arbitrary
    pure Stop{..}

arbitraryGroupSymProperties :: FontSetMap -> Gen GroupSymProperties
arbitraryGroupSymProperties fontMap = do
  layout <- arbitrary
  rules <- resize 3 (listOf (arbitraryGroupRule fontMap))
  return GroupSymProperties{..}

instance Arbitrary GroupLayout where
  arbitrary = oneof [ SimpleRowLayout <$> arbitrary
                    , PairLayout      <$> arbitrary <*> arbitrary
                    ]

arbitraryGroupRule :: FontSetMap -> Gen GroupRule
arbitraryGroupRule fontMap = do
  symbolizers <- resize 3 (listOf (arbitrarySymbolizer fontMap))
  filter      <- arbitrary
  repeatKey   <- arbitrary
  return GroupRule{..}

arbitraryPlacements :: FontSetMap -> Gen TextPlacements
arbitraryPlacements fontMap = oneof [ dummy, simple, list ]
  where
    dummy = Dummy <$> arbitraryTextSymProperties fontMap
    simple = Simple <$> arbitraryTextSymProperties fontMap
                    <*> (Val <$> arbitrary)
    list = List <$> arbitraryTextSymProperties fontMap
                <*> listOf (arbitraryTextSymProperties fontMap)

arbitraryTextSymProperties :: FontSetMap -> Gen TextSymProperties
arbitraryTextSymProperties fontMap = do
  properties       <- arbitrary
  layoutProperties <- arbitrary
  formatProperties <- arbitraryTextFormatProperties fontMap
  format           <- arbitraryFormat fontMap
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

instance Arbitrary SimplePlacementPosition where
  arbitrary = do
    sizes <- listOf (getPositive <$> arbitrary)
    pos <- if null sizes then listOf1 arbitrary else arbitrary
    pure (SimplePlacementPosition sizes pos)

instance Arbitrary TextLayoutProperties where
  arbitrary = do
    dx                  <- arbitrary
    dy                  <- arbitrary
    orientation         <- arbitrary
    textRatio           <- arbitrary
    wrapWidth           <- arbitrary
    wrapChar            <- maybeArb (arbitraryPropWith arbitraryPrintableChar)
    wrapBefore          <- arbitrary
    repeatWrapChar      <- arbitrary
    rotateDisplacement  <- arbitrary
    horizontalAlignment <- arbitrary
    justifyAlignment    <- arbitrary
    verticalAlignment   <- arbitrary
    direction           <- arbitrary
    pure TextLayoutProperties{..}

arbitraryTextFormatProperties :: FontSetMap -> Gen TextFormatProperties
arbitraryTextFormatProperties fontMap = do
  font             <- maybeArb (arbitraryFont fontMap)
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

arbitraryFont :: FontSetMap -> Gen Font
arbitraryFont fontMap | M.null fontMap = FaceName <$> arbitrary
arbitraryFont fontMap = oneof
  [ FaceName <$> arbitrary
  , FontSetName <$> elements  (M.keys fontMap)
  ]

arbitraryFormat :: FontSetMap -> Gen Format
arbitraryFormat fontMap = oneof
  [ sized arbitraryF
  , arbitraryFormatList
  , arbitraryFormatExp
  , sized arbitraryFormatLayout
  , pure NullFormat
  ]
  where
  arbitraryF :: Int -> Gen Format
  arbitraryF 0 = pure NullFormat
  arbitraryF n = do
    font             <- maybeArb (arbitraryFont fontMap)
    textSize         <- arbitrary
    opacity          <- arbitrary
    characterSpacing <- arbitrary
    lineSpacing      <- arbitrary
    wrapBefore       <- arbitrary
    repeatWrapChar   <- arbitrary
    textTransform    <- arbitrary
    fill             <- arbitrary
    haloFill         <- arbitrary
    haloRadius       <- arbitrary
    ffSettings       <- arbitrary
    next             <- resize (n-1) (arbitraryFormat fontMap)
    pure Format{..}

  arbitraryFormatLayout :: Int -> Gen Format
  arbitraryFormatLayout 0 = pure NullFormat
  arbitraryFormatLayout n = do
    dx                  <- arbitrary
    dy                  <- arbitrary
    orientation         <- arbitrary
    textRatio           <- arbitrary
    wrapWidth           <- arbitrary
    wrapChar            <- maybeArb (arbitraryPropWith arbitraryPrintableChar)
    wrapBefore          <- arbitrary
    repeatWrapChar      <- arbitrary
    rotateDisplacement  <- arbitrary
    horizontalAlignment <- arbitrary
    justifyAlignment    <- arbitrary
    verticalAlignment   <- arbitrary
    next                <- resize (n-1) (arbitraryFormat fontMap)
    pure FormatLayout{..}

  arbitraryFormatList :: Gen Format
  arbitraryFormatList = FormatList <$> listOf (oneof
      [ sized arbitraryF
      , arbitraryFormatExp
      , sized arbitraryFormatLayout
      ])

  arbitraryFormatExp :: Gen Format
  arbitraryFormatExp = FormatExp <$> arbitrary

instance Arbitrary Font where
  arbitrary = oneof [FaceName <$> arbitraryFaceName, FontSetName <$> arbitrary]
    where
      arbitraryFaceName = elements
        [ "DejaVu Sans Book"
        ]


instance Arbitrary FontFeatureSettings where
  arbitrary = FontFeatureSettings <$> parts
    where
      parts = oneof
        [ pure "smcp"
        , pure "hist"
        , pure "frac"
        ]

instance Arbitrary Rule where arbitrary = scale (min 2) (arbitraryRule mempty)
instance Arbitrary TextPlacements where arbitrary = scale (min 2) (arbitraryPlacements mempty)
instance Arbitrary GroupRule where arbitrary = scale (min 2) (arbitraryGroupRule mempty)
instance Arbitrary GroupSymProperties where arbitrary = scale (min 2) (arbitraryGroupSymProperties mempty)
instance Arbitrary TextSymProperties where arbitrary = scale (min 2) (arbitraryTextSymProperties mempty)
instance Arbitrary TextFormatProperties where arbitrary = scale (min 2) (arbitraryTextFormatProperties mempty)
instance Arbitrary Symbolizer where arbitrary = scale (min 2) (arbitrarySymbolizer mempty)
instance Arbitrary Style where arbitrary = scale (min 2) (arbitraryStyle mempty)
    

-----------------------------------------------------------------------------
arbitraryEnum :: (Enum a, Bounded a) => Gen a
arbitraryEnum = elements [minBound..maxBound]

arbitrarySrs :: Gen Proj4
arbitrarySrs = elements allSrs

allSrs :: [Proj4]
allSrs = unsafePerformIO $
      P.filter (not . T.all (==' ')) . T.lines
  <$> (T.readFile =<< getDataFileName "spec/proj4s.txt")
{-# NOINLINE allSrs #-}

arbitraryMinMaxScaleDenom :: Gen (Maybe Double, Maybe Double)
arbitraryMinMaxScaleDenom = do
  min_ <- maybeArb (getPositive <$> arbitrary)
  max_ <- case min_ of
    Just min' -> maybeArb ((+min') . getNonNegative <$> arbitrary)
    Nothing   -> maybeArb (getPositive <$> arbitrary)
  pure (min_, max_)

arbitraryLayer :: Arbitrary s => [StyleName] -> Gen (Layer s)
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

instance Arbitrary v => Arbitrary (M.HashMap T.Text v) where
  arbitrary = sized $ \n ->
    M.fromList <$> (zip <$> vectorOf n arbitraryText <*> arbitrary)

instance Arbitrary T.Text where
  arbitrary = arbitraryText

arbitraryText :: Gen T.Text
arbitraryText =
  sized $ \n -> T.pack <$> vectorOf n arbitraryPrintableChar

arbitraryPrintableChar :: Gen Char
arbitraryPrintableChar = do
  c <- arbitrary
  if isPrint c then pure c else arbitraryPrintableChar
