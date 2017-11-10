{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Mapnik.Bindings.Symbolizer (
  Symbolizer
, create
, unCreate
, unsafeNew
) where

import           Mapnik.Lens
import qualified Mapnik
import           Mapnik.Enums
import           Mapnik.Symbolizer.Property
import           Mapnik ( Transform(..), Prop (..) )
import           Mapnik.Bindings hiding (TextPlacements(..))
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Orphans ()
import qualified Mapnik.Bindings.Expression as Expression
import qualified Mapnik.Bindings.Transform as Transform

import           Control.Exception
import           Control.Lens hiding (has)
import           Data.Maybe (catMaybes)
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.String (fromString)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.Marshal.Utils (with)
import           Foreign.Marshal.Alloc (finalizerFree)
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Storable         as V

import qualified Language.C.Inline.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/symbolizer.hpp>"
C.include "<mapnik/symbolizer_utils.hpp>"
C.include "<mapnik/symbolizer_keys.hpp>"
C.include "<mapnik/expression_string.hpp>"
C.include "<mapnik/expression_evaluator.hpp>"
C.include "<mapnik/transform_processor.hpp>"
C.include "symbolizer_util.hpp"

C.using "namespace mapnik"

--
-- * Symbolizer


foreign import ccall "&hs_mapnik_destroy_Symbolizer" destroySymbolizer :: FinalizerPtr Symbolizer

unsafeNew :: Ptr Symbolizer -> IO Symbolizer
unsafeNew = fmap Symbolizer . newForeignPtr destroySymbolizer

create :: Mapnik.Symbolizer -> IO Symbolizer
create sym = bracket alloc dealloc $ \p -> do
  mapM_ (`setProperty` p) (sym^.properties)
  unsafeNew =<< castSym sym p
  where
    alloc = [C.exp|symbolizer_base * { new symbolizer_base() }|]
    dealloc p = [C.block|void { delete $(symbolizer_base *p);}|]

unCreate :: Symbolizer -> IO Mapnik.Symbolizer
unCreate sym' = bracket alloc dealloc $ \sym -> do
  props <- catMaybes <$> sequence
    [ getProperty Gamma sym
    , getProperty GammaMethod sym
    , getProperty Opacity sym
    , getProperty Alignment sym
    , getProperty Offset sym
    , getProperty CompOp sym
    , getProperty Clip sym
    , getProperty Fill sym
    , getProperty FillOpacity sym
    , getProperty Stroke sym
    , getProperty StrokeWidth sym
    , getProperty StrokeOpacity sym
    , getProperty StrokeLinejoin sym
    , getProperty StrokeLinecap sym
    , getProperty StrokeGamma sym
    , getProperty StrokeGammaMethod sym
    , getProperty StrokeDashoffset sym
    , getProperty StrokeDasharray sym
    , getProperty StrokeMiterlimit sym
    , getProperty GeometryTransform sym
    , getProperty LineRasterizer sym
    , getProperty ImageTransform sym
    , getProperty Spacing sym
    , getProperty MaxError sym
    , getProperty AllowOverlap sym
    , getProperty IgnorePlacement sym
    , getProperty Width sym
    , getProperty Height sym
    , getProperty File sym
    , getProperty ShieldDx sym
    , getProperty ShieldDy sym
    , getProperty UnlockImage sym
    , getProperty Mode sym
    , getProperty Scaling sym
    , getProperty FilterFactor sym
    , getProperty MeshSize sym
    , getProperty Premultiplied sym
    , getProperty Smooth sym
    , getProperty SimplifyAlgorithm sym
    , getProperty SimplifyTolerance sym
    , getProperty HaloRasterizer sym
    , getProperty TextPlacements sym
    , getProperty LabelPlacement sym
    , getProperty MarkersPlacementType sym
    , getProperty MarkersMultipolicy sym
    , getProperty PointPlacementType sym
    , getProperty Colorizer sym
    , getProperty HaloTransform sym
    , getProperty NumColumns sym
    , getProperty StartColumn sym
    , getProperty RepeatKey sym
    , getProperty GroupProperties sym
    , getProperty LargestBoxOnly sym
    , getProperty MinimumPathLength sym
    , getProperty HaloCompOp sym
    , getProperty TextTransform sym
    , getProperty HorizontalAlignment sym
    , getProperty JustifyAlignment sym
    , getProperty VerticalAlignment sym
    , getProperty Upright sym
    , getProperty Direction sym
    , getProperty AvoidEdges sym
    , getProperty FfSettings sym
    ]
  symName <- getName sym'
  case defFromName symName of
    Just s -> return (s & properties .~ props)
    Nothing -> throwIO (userError ("Unexpected symbolizer name: " ++ unpack symName))
  where
    alloc = [C.exp|symbolizer_base * { get_symbolizer_base(*$fptr-ptr:(symbolizer *sym')) }|]
    dealloc p = [C.block|void { delete $(symbolizer_base *p);}|]

getName :: Symbolizer -> IO Text
getName s = newText $ \(ptr, len) ->
  [C.block|void {
  std::string s = symbolizer_name(*$fptr-ptr:(symbolizer *s));
  *$(char** ptr)= strdup(s.c_str());
  *$(int* len) = s.length();
  }|]

defFromName :: Text -> Maybe Mapnik.Symbolizer
defFromName "PointSymbolizer"          = Just Mapnik.point
defFromName "LineSymbolizer"           = Just Mapnik.line
defFromName "LinePatternSymbolizer"    = Just Mapnik.linePattern
defFromName "PolygonSymbolizer"        = Just Mapnik.polygon
defFromName "PolygonPatternSymbolizer" = Just Mapnik.polygonPattern
defFromName "RasterSymbolizer"         = Just Mapnik.raster
defFromName "ShieldSymbolizer"         = Just Mapnik.shield
defFromName "TextSymbolizer"           = Just Mapnik.text
defFromName "BuildingSymbolizer"       = Just Mapnik.building
defFromName "MarkersSymbolizer"        = Just Mapnik.markers
defFromName "GroupSymbolizer"          = Just Mapnik.group
defFromName "DebugSymbolizer"          = Just Mapnik.debug
defFromName "DotSymbolizer"            = Just Mapnik.dot
defFromName _                          = Nothing

castSym :: Mapnik.Symbolizer
        -> Ptr SymbolizerBase -> IO (Ptr Symbolizer)
castSym Mapnik.Point{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<point_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.Line{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<line_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.LinePattern{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<line_pattern_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.Polygon{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<polygon_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.PolygonPattern{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<polygon_pattern_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.Raster{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<raster_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.Shield{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<shield_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.Text{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<text_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.Building{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<building_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.Markers{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<markers_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.Group{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<group_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.Debug{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<debug_symbolizer*>($(symbolizer_base *p)));}|]
castSym Mapnik.Dot{} p =
  [C.block|symbolizer *{ new symbolizer(*static_cast<dot_symbolizer*>($(symbolizer_base *p)));}|]


class HasSetProp a where
  setProp :: Key a -> a -> Ptr SymbolizerBase -> IO ()

class HasGetProp a where
  getProp :: Key a -> Ptr SymbolizerBase -> IO (Maybe a)

data Property where
  (:=>) :: HasSetProp v => Key v -> Prop v -> Property

type Properties = [Property]

data Key a where
    Gamma :: Key Double
    GammaMethod :: Key GammaMethod
    Opacity :: Key Double
    Alignment :: Key PatternAlignment
    Offset :: Key Double
    CompOp :: Key CompositeMode
    Clip :: Key Bool
    Fill :: Key Mapnik.Color
    FillOpacity :: Key Double
    Stroke :: Key Mapnik.Color
    StrokeWidth :: Key Double
    StrokeOpacity :: Key Double
    StrokeLinejoin :: Key LineJoin
    StrokeLinecap :: Key LineCap
    StrokeGamma :: Key Double
    StrokeGammaMethod :: Key GammaMethod
    StrokeDashoffset :: Key Double
    StrokeDasharray :: Key Mapnik.DashArray
    StrokeMiterlimit :: Key Double
    GeometryTransform :: Key Mapnik.Transform
    LineRasterizer :: Key LineRasterizer
    ImageTransform :: Key Mapnik.Transform
    Spacing :: Key Double
    MaxError :: Key Double
    AllowOverlap :: Key Bool
    IgnorePlacement :: Key Bool
    Width :: Key Double
    Height :: Key Double
    File :: Key FilePath
    ShieldDx :: Key Double
    ShieldDy :: Key Double
    UnlockImage :: Key Bool
    Mode :: Key (Either DebugMode RasterMode)
    Scaling :: Key Scaling
    FilterFactor :: Key Double
    MeshSize :: Key Int
    Premultiplied :: Key Bool
    Smooth :: Key Double
    SimplifyAlgorithm :: Key SimplifyAlgorithm
    SimplifyTolerance :: Key Double
    HaloRasterizer :: Key HaloRasterizer
    TextPlacements :: Key TextPlacements
    LabelPlacement :: Key LabelPlacement
    MarkersPlacementType :: Key MarkerPlacement
    MarkersMultipolicy :: Key MarkerMultiPolicy
    PointPlacementType :: Key PointPlacement
    Colorizer :: Key Colorizer
    HaloTransform :: Key Mapnik.Transform
    NumColumns :: Key Int
    StartColumn :: Key Int
    RepeatKey :: Key Mapnik.Expression
    GroupProperties :: Key GroupProperties
    LargestBoxOnly :: Key Bool
    MinimumPathLength :: Key Double
    HaloCompOp :: Key CompositeMode
    TextTransform :: Key TextTransform
    HorizontalAlignment :: Key HorizontalAlignment
    JustifyAlignment :: Key JustifyAlignment
    VerticalAlignment :: Key VerticalAlignment
    Upright :: Key Upright
    Direction :: Key Direction
    AvoidEdges :: Key Bool
    FfSettings :: Key FontFeatureSettings


class HasProperties s a | s -> a where
  properties :: Lens' s a

instance HasProperties Mapnik.Symbolizer Properties where
  properties = lens getProps setProps where
    setProps sym@Mapnik.Point{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (File               :=> v) = file ?~ v
      step (Opacity            :=> v) = opacity ?~ v
      step (AllowOverlap       :=> v) = allowOverlap ?~ v
      step (IgnorePlacement    :=> v) = ignorePlacement ?~ v
      step (PointPlacementType :=> v) = pointPlacement ?~ v
      step (ImageTransform     :=> v) = imageTransform ?~ v
      step p                          = stepBase p

    setProps sym@Mapnik.Line{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (Opacity :=> v) = opacity ?~ v
      step (Offset  :=> v) = offset ?~ v
      step p               = stepStroke p

    setProps sym@Mapnik.LinePattern{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (File            :=> v) = file ?~ v
      step (Opacity         :=> v) = opacity ?~ v
      step (Offset          :=> v) = offset ?~ v
      step (ImageTransform  :=> v) = imageTransform ?~ v
      step p                       = stepBase p

    setProps sym@Mapnik.Polygon{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (Fill        :=> v) = fill ?~ v
      step (FillOpacity :=> v) = fillOpacity ?~ v
      step (Gamma       :=> v) = gamma ?~ v
      step (GammaMethod :=> v) = gammaMethod ?~ v
      step p                   = stepBase p

    setProps sym@Mapnik.PolygonPattern{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (File           :=> v) = file ?~ v
      step (Opacity        :=> v) = opacity ?~ v
      step (Gamma          :=> v) = gamma ?~ v
      step (GammaMethod    :=> v) = gammaMethod ?~ v
      step (ImageTransform :=> v) = imageTransform ?~ v
      step (Alignment      :=> v) = alignment ?~ v
      step p                      = stepBase p

    setProps sym@Mapnik.Raster{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (Mode          :=> Val (Right v)) = rasterMode ?~ v
      step (Scaling       :=> Val v        ) = scaling ?~ v
      step (Opacity       :=> Val v        ) = rasterOpacity ?~ v
      step (FilterFactor  :=> Val v        ) = filterFactor ?~ v
      step (MeshSize      :=> Val v        ) = meshSize ?~ v
      step (Premultiplied :=> Val v        ) = preMultiplied ?~ v
      step (Colorizer     :=> Val v        ) = colorizer ?~ v
      step p                                 = stepBase p

    setProps sym@Mapnik.Shield{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (TextPlacements :=> v) = placements ?~ v
      step (ImageTransform :=> v) = imageTransform ?~ v
      step (ShieldDx       :=> v) = dx ?~ v
      step (ShieldDy       :=> v) = dy ?~ v
      step (Opacity        :=> v) = opacity ?~ v
      step (UnlockImage    :=> v) = unlockImage ?~ v
      step (File           :=> v) = file ?~ v
      step (HaloRasterizer :=> v) = haloRasterizer ?~ v
      step p                      = stepBase p

    setProps sym@Mapnik.Text{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (TextPlacements :=> v) = placements ?~ v
      step (HaloCompOp     :=> v) = haloCompOp ?~ v
      step (HaloRasterizer :=> v) = haloRasterizer ?~ v
      step (HaloTransform  :=> v) = haloTransform ?~ v
      step p                      = stepBase p

    setProps sym@Mapnik.Building{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (Fill        :=> v) = fill ?~ v
      step (FillOpacity :=> v) = fillOpacity ?~ v
      step (Height      :=> v) = height ?~ v
      step p                   = stepBase p

    setProps sym@Mapnik.Markers{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (File                 :=> v) = file ?~ v
      step (Opacity              :=> v) = opacity ?~ v
      step (Fill                 :=> v) = fill ?~ v
      step (FillOpacity          :=> v) = fillOpacity ?~ v
      step (Spacing              :=> v) = spacing ?~ v
      step (MaxError             :=> v) = maxError ?~ v
      step (Offset               :=> v) = offset ?~ v
      step (Width                :=> v) = width ?~ v
      step (Height               :=> v) = height ?~ v
      step (AllowOverlap         :=> v) = allowOverlap ?~ v
      step (AvoidEdges           :=> v) = avoidEdges ?~ v
      step (IgnorePlacement      :=> v) = ignorePlacement ?~ v
      step (ImageTransform       :=> v) = imageTransform ?~ v
      step (MarkersPlacementType :=> v) = placement ?~ v
      step (MarkersMultipolicy   :=> v) = multiPolicy ?~ v
      step (Direction            :=> v) = direction ?~ v
      step p                            = stepStroke p

    setProps sym@Mapnik.Group{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (GroupProperties :=> v) = groupProperties ?~ v
      step (NumColumns      :=> v) = numColumns ?~ v
      step (StartColumn     :=> v) = startColumn ?~ v
      step (RepeatKey       :=> v) = repeatKey ?~ v
      step (TextPlacements  :=> v) = placements ?~ v
      step p                       = stepBase p

    setProps sym@Mapnik.Debug{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (Mode :=> Val (Left v)) = debugMode ?~ v
      step p                       = stepBase p

    setProps sym@Mapnik.Dot{} = foldr step sym  where
      step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
      step (Fill    :=> v) = fill ?~ v
      step (Opacity :=> v) = opacity ?~ v
      step (Width   :=> v) = width ?~ v
      step (Height  :=> v) = height ?~ v
      step (CompOp  :=> v) = compOp ?~ v
      step _               = id

    stepBase, stepStroke :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    stepBase (SimplifyTolerance :=> v) = simplifyTolerance ?~ v
    stepBase (Smooth            :=> v) = smooth ?~ v
    stepBase (Clip              :=> v) = clip ?~ v
    stepBase (CompOp            :=> v) = compOp ?~ v
    stepBase (GeometryTransform :=> v) = geometryTransform ?~ v
    stepBase (SimplifyAlgorithm :=> v) = simplifyAlgorithm ?~ v
    stepBase _                         = id

    stepStroke (StrokeGamma       :=> v) = strokeGamma ?~ v
    stepStroke (StrokeGammaMethod :=> v) = strokeGammaMethod ?~ v
    stepStroke (StrokeDasharray   :=> v) = strokeDashArray ?~ v
    stepStroke (StrokeDashoffset  :=> v) = strokeDashOffset ?~ v
    stepStroke (StrokeMiterlimit  :=> v) = strokeMiterLimit ?~ v
    stepStroke (StrokeWidth       :=> v) = strokeWidth ?~ v
    stepStroke (StrokeOpacity     :=> v) = strokeOpacity ?~ v
    stepStroke (Stroke            :=> v) = stroke ?~ v
    stepStroke (StrokeLinejoin    :=> v) = strokeLineJoin ?~ v
    stepStroke (StrokeLinecap     :=> v) = strokeLineCap ?~ v
    stepStroke p                         = stepBase p

#define GET_BASE_PROPS \
    fmap (SimplifyTolerance :=>) (sym^?!simplifyTolerance) \
  , fmap (Smooth            :=>) (sym^?!smooth)            \
  , fmap (Clip              :=>) (sym^?!clip) \
  , fmap (CompOp            :=>) (sym^?!compOp) \
  , fmap (GeometryTransform :=>) (sym^?!geometryTransform) \
  , fmap (SimplifyAlgorithm :=>) (sym^?!simplifyAlgorithm)

#define GET_STROKE_PROPS \
    fmap (StrokeGamma       :=>) (sym^?!strokeGamma) \
  , fmap (StrokeGammaMethod :=>) (sym^?!strokeGammaMethod) \
  , fmap (StrokeDasharray   :=>) (sym^?!strokeDashArray) \
  , fmap (StrokeDashoffset  :=>) (sym^?!strokeDashOffset) \
  , fmap (StrokeMiterlimit  :=>) (sym^?!strokeMiterLimit) \
  , fmap (StrokeWidth       :=>) (sym^?!strokeWidth) \
  , fmap (StrokeOpacity     :=>) (sym^?!strokeOpacity) \
  , fmap (Stroke            :=>) (sym^?!stroke) \
  , fmap (StrokeLinejoin    :=>) (sym^?!strokeLineJoin) \
  , fmap (StrokeLinecap     :=>) (sym^?!strokeLineCap) \
  , GET_BASE_PROPS

    getProps sym = catMaybes $ case sym of
      Mapnik.Point{} ->
        [ fmap (File               :=>) (sym^?!file)
        , fmap (Opacity            :=>) (sym^?!opacity)
        , fmap (AllowOverlap       :=>) (sym^?!allowOverlap)
        , fmap (IgnorePlacement    :=>) (sym^?!ignorePlacement)
        , fmap (PointPlacementType :=>) (sym^?!pointPlacement)
        , fmap (ImageTransform     :=>) (sym^?!imageTransform)
        , GET_BASE_PROPS
        ]
      Mapnik.Line{} ->
        [ fmap (Offset         :=>) (sym^?!offset)
        , fmap (LineRasterizer :=>) (sym^?!lineRasterizer)
        , GET_STROKE_PROPS
        ]
      Mapnik.LinePattern{} ->
        [ fmap (File           :=>) (sym^?!file)
        , fmap (Opacity        :=>) (sym^?!opacity)
        , fmap (Offset         :=>) (sym^?!offset)
        , fmap (ImageTransform :=>) (sym^?!imageTransform)
        , GET_BASE_PROPS
        ]
      Mapnik.Polygon{} ->
        [ fmap (Fill        :=>) (sym^?!fill)
        , fmap (FillOpacity :=>) (sym^?!fillOpacity)
        , fmap (Gamma       :=>) (sym^?!gamma)
        , fmap (GammaMethod :=>) (sym^?!gammaMethod)
        , GET_BASE_PROPS
        ]
      Mapnik.PolygonPattern{} ->
        [ fmap (File           :=>) (sym^?!file)
        , fmap (Opacity        :=>) (sym^?!opacity)
        , fmap (Gamma          :=>) (sym^?!gamma)
        , fmap (GammaMethod    :=>) (sym^?!gammaMethod)
        , fmap (ImageTransform :=>) (sym^?!imageTransform)
        , fmap (Alignment      :=>) (sym^?!alignment)
        , GET_BASE_PROPS
        ]
      Mapnik.Raster{} ->
        [ fmap ((Mode :=>) . Val . Right)  (sym^?!rasterMode)
        , fmap ((Scaling       :=>) . Val) (sym^?!scaling)
        , fmap ((Opacity       :=>) . Val) (sym^?!rasterOpacity)
        , fmap ((FilterFactor  :=>) . Val) (sym^?!filterFactor)
        , fmap ((MeshSize      :=>) . Val) (sym^?!meshSize)
        , fmap ((Premultiplied :=>) . Val) (sym^?!preMultiplied)
        , fmap ((Colorizer     :=>) . Val) (sym^?!colorizer)
        , GET_BASE_PROPS
        ]
      Mapnik.Shield{} ->
        [ fmap (TextPlacements    :=>) (sym^?!placements)
        , fmap (GeometryTransform :=>) (sym^?!imageTransform)
        , fmap (ShieldDx          :=>) (sym^?!dx)
        , fmap (ShieldDy          :=>) (sym^?!dy)
        , fmap (Opacity           :=>) (sym^?!opacity)
        , fmap (UnlockImage       :=>) (sym^?!unlockImage)
        , fmap (File              :=>) (sym^?!file)
        , fmap (HaloRasterizer    :=>) (sym^?!haloRasterizer)
        , GET_BASE_PROPS
        ]
      Mapnik.Text{} ->
        [ fmap (TextPlacements    :=>) (sym^?!placements)
        , fmap (HaloCompOp        :=>) (sym^?!haloCompOp)
        , fmap (HaloRasterizer    :=>) (sym^?!haloRasterizer)
        , fmap (GeometryTransform :=>) (sym^?!haloTransform)
        , GET_BASE_PROPS
        ]
      Mapnik.Building{} ->
        [ fmap (Fill        :=>) (sym^?!fill)
        , fmap (FillOpacity :=>) (sym^?!fillOpacity)
        , fmap (Height      :=>) (sym^?!height)
        , GET_BASE_PROPS
        ]
      Mapnik.Markers{} ->
        [ fmap (File                 :=>) (sym^?!file)
        , fmap (Opacity              :=>) (sym^?!opacity)
        , fmap (Fill                 :=>) (sym^?!fill)
        , fmap (FillOpacity          :=>) (sym^?!fillOpacity)
        , fmap (Spacing              :=>) (sym^?!spacing)
        , fmap (MaxError             :=>) (sym^?!maxError)
        , fmap (Offset               :=>) (sym^?!offset)
        , fmap (Width                :=>) (sym^?!width)
        , fmap (Height               :=>) (sym^?!height)
        , fmap (AllowOverlap         :=>) (sym^?!allowOverlap)
        , fmap (AvoidEdges           :=>) (sym^?!avoidEdges)
        , fmap (IgnorePlacement      :=>) (sym^?!ignorePlacement)
        , fmap (GeometryTransform    :=>) (sym^?!imageTransform)
        , fmap (MarkersPlacementType :=>) (sym^?!placement)
        , fmap (MarkersMultipolicy   :=>) (sym^?!multiPolicy)
        , fmap (Direction            :=>) (sym^?!direction)
        , GET_STROKE_PROPS
        ]
      Mapnik.Group{} ->
        [ fmap (GroupProperties :=>) (sym^?!groupProperties)
        , fmap (NumColumns      :=>) (sym^?!numColumns)
        , fmap (StartColumn     :=>) (sym^?!startColumn)
        , fmap (RepeatKey       :=>) (sym^?!repeatKey)
        , fmap (TextPlacements  :=>) (sym^?!placements)
        , GET_BASE_PROPS
        ]
      Mapnik.Debug{} ->
        [ fmap ((Mode :=>) . Val . Left)  (sym^?!debugMode)
        , GET_BASE_PROPS
        ]
      Mapnik.Dot{} ->
        [ fmap (Fill    :=>) (sym^?!fill)
        , fmap (Opacity :=>) (sym^?!opacity)
        , fmap (Width   :=>) (sym^?!width)
        , fmap (Height  :=>) (sym^?!height)
        , fmap (CompOp  :=>) (sym^?!compOp)
        ]



instance HasGetProp Double where
  getProp (keyIndex -> k) sym = fmap (fmap realToFrac) <$> newMaybe $ \(has,p) ->
    [C.block|void {
    auto val = get_optional<double>(*$(symbolizer_base *sym), $(keys k));
    if (val) {
      *$(int *has) = 1;
      *$(double *p) = *val;
    } else {
      *$(int *has) = 0;
    }
    }|]

instance HasSetProp Double where
  setProp (keyIndex -> k) (realToFrac -> v) s = 
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = $(double v);}|]

instance HasSetProp () where setProp _ _ _ = return ()
instance HasGetProp () where getProp _ _ = return Nothing

instance HasGetProp Mapnik.Color where
  getProp (keyIndex -> k) sym = newMaybe $ \(has,ret) ->
    [C.block|void {
    auto val = get_optional<color>(*$(symbolizer_base *sym), $(keys k));
    if (val) {
      *$(int *has) = 1;
      *$(color *ret) = *val;
    } else {
      *$(int *has) = 0;
    }
    }|]

instance HasSetProp Mapnik.Color where
  setProp (keyIndex -> k) c s = with c $ \cPtr ->
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = *$(color *cPtr);}|]

instance HasGetProp Int where
  getProp (keyIndex -> k) sym = fmap (fmap fromIntegral) <$> newMaybe $ \(has,p) ->
    [C.block|void {
    auto val = get_optional<int>(*$(symbolizer_base *sym), $(keys k));
    if (val) {
      *$(int *has) = 1;
      *$(int *p) = *val;
    } else {
      *$(int *has) = 0;
    }
    }|]

instance HasSetProp Int where
  setProp (keyIndex -> k) (fromIntegral -> v) s =
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = $(int v);}|]

instance HasGetProp Text where
  getProp (keyIndex -> k) sym = newTextMaybe $ \(ptr,len) ->
    [C.block|void {
    auto val = get_optional<std::string>(*$(symbolizer_base *sym), $(keys k));
    if (val) {
      *$(char **ptr) = strdup(val->c_str());
      *$(int *len) = val->size();
    } else {
      *$(char **ptr) = NULL;
    }
    }|]

instance HasSetProp Text where
  setProp (keyIndex -> k) (encodeUtf8 -> v) s =
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = std::string($bs-ptr:v, $bs-len:v);}|]

instance HasGetProp String where
  getProp (keyIndex -> k) sym = fmap (fmap unpack) $ newTextMaybe $ \(ptr,len) ->
    [C.block|void {
    auto val = get_optional<std::string>(*$(symbolizer_base *sym), $(keys k));
    if (val) {
      *$(char **ptr) = strdup(val->c_str());
      *$(int *len) = val->size();
    } else {
      *$(char **ptr) = NULL;
    }
    }|]

instance HasSetProp String where
  setProp (keyIndex -> k) (fromString -> v) s =
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = std::string($bs-ptr:v, $bs-len:v);}|]


instance HasGetProp Mapnik.Expression where getProp = getPropExpression
instance HasSetProp Mapnik.Expression where setProp = setPropExpression

instance HasSetProp Mapnik.Transform where
  setProp (keyIndex -> k) (Mapnik.Transform expr) s =
    case Transform.parse expr of
      Right v ->
        [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = *$fptr-ptr:(transform_type *v);}|]
      Left e -> throwIO (userError e)

instance HasGetProp Mapnik.Transform where
  getProp (keyIndex -> k) sym =
    fmap (fmap Mapnik.Transform) $ newTextMaybe $ \(ptr, len) ->
      [C.block|void {
      auto expr = get_optional<transform_type>(*$(symbolizer_base *sym), $(keys k));
      if (expr && *expr) {
        std::string s = transform_processor_type::to_string(**expr);
        *$(char** ptr) = strdup(s.c_str());
        *$(int* len) = s.length();
      } else {
        *$(char** ptr) = NULL;
      }
      }|]

instance HasSetProp Mapnik.DashArray where
  setProp (keyIndex -> k) dashes s = undefined

instance HasGetProp Mapnik.DashArray where
  getProp (keyIndex -> k) sym = do
    (has, fromIntegral -> len, castPtr -> ptr) <- C.withPtrs_ $ \(has, len,ptr) ->
      [C.block|void{
      auto arr = get_optional<dash_array>(*$(symbolizer_base *sym), $(keys k));
      if (arr) {
        *$(int *has) = 1;
        *$(size_t *len) = arr->size();
        double *dashes = *$(double **ptr) =
          static_cast<double *>(malloc(arr->size()*2*sizeof(double)));
        int i=0;
        for (dash_array::const_iterator it=arr->begin(); it!=arr->end(); ++it, ++i) {
          dashes[i*2]   = it->first;
          dashes[i*2+1] = it->second;
        }
      } else {
        *$(int *has) = 0;
      }
      }|]
    if has==1 then do
      fp <- newForeignPtr finalizerFree ptr
      Just <$> V.freeze (VM.unsafeFromForeignPtr0 fp len)
    else return Nothing

instance HasGetProp (Either DebugMode RasterMode) where getProp key sym = return Nothing --TODO
instance HasSetProp (Either DebugMode RasterMode) where setProp key val sym = undefined

#define HAS_GET_PROP_ENUM(HS,CPP) \
instance HasGetProp HS where {\
  getProp (keyIndex -> k) sym = fmap (fmap (toEnum . fromIntegral)) <$> newMaybe $ \(has,p) -> \
    [C.block|void { \
    auto val = get_optional<CPP>(*$(symbolizer_base *sym), $(keys k)); \
    if (val) { \
      *$(int *has) = 1; \
      *$(int *p) = static_cast<int>(*val); \
    } else { \
      *$(int *has) = 0; \
    } \
    }|] \
};\
instance HasSetProp HS where \
  setProp (keyIndex -> k) (fromIntegral . fromEnum -> v) s = \
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = enumeration_wrapper(static_cast<CPP>($(int v)));}|]

HAS_GET_PROP_ENUM(Bool,bool)
HAS_GET_PROP_ENUM(CompositeMode,composite_mode_e)
HAS_GET_PROP_ENUM(LineCap,line_cap_enum)
HAS_GET_PROP_ENUM(LineJoin,line_join_enum)
HAS_GET_PROP_ENUM(LineRasterizer,line_rasterizer_enum)
HAS_GET_PROP_ENUM(HaloRasterizer,halo_rasterizer_enum)
HAS_GET_PROP_ENUM(PointPlacement,point_placement_enum)
HAS_GET_PROP_ENUM(PatternAlignment,pattern_alignment_enum)
HAS_GET_PROP_ENUM(DebugMode,debug_symbolizer_mode_enum)
HAS_GET_PROP_ENUM(MarkerPlacement,marker_placement_enum)
HAS_GET_PROP_ENUM(MarkerMultiPolicy,marker_multi_policy_enum)
HAS_GET_PROP_ENUM(TextTransform,text_transform_enum)
HAS_GET_PROP_ENUM(LabelPlacement,label_placement_enum)
HAS_GET_PROP_ENUM(VerticalAlignment,vertical_alignment_enum)
HAS_GET_PROP_ENUM(HorizontalAlignment,horizontal_alignment_enum)
HAS_GET_PROP_ENUM(JustifyAlignment,justify_alignment_enum)
HAS_GET_PROP_ENUM(Upright,text_upright_enum)
HAS_GET_PROP_ENUM(Direction,direction_enum)
HAS_GET_PROP_ENUM(GammaMethod,gamma_method_enum)


getProperty :: (HasSetProp a, HasGetProp a)
            => Key a -> Ptr SymbolizerBase -> IO (Maybe Property)
getProperty k p = do
  eVal <- getPropExpression k p
  case eVal of
    Nothing -> fmap ((k :=>) . Val) <$> getProp k p
    Just e -> return (Just (k :=> Exp e))

setProperty :: Property -> Ptr SymbolizerBase -> IO ()
setProperty (k :=> Val v) = setProp k v
setProperty (k :=> Exp v) = setPropExpression k v

setPropExpression :: Key a
                  -> Mapnik.Expression -> Ptr SymbolizerBase -> IO ()
setPropExpression (keyIndex -> k) (Mapnik.Expression expr) s =
  case Expression.parse expr of
    Right v ->
      [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = *$fptr-ptr:(expression_ptr *v);}|]
    Left e -> throwIO (userError e)

getPropExpression :: Key a
                  -> Ptr SymbolizerBase -> IO (Maybe Mapnik.Expression)
getPropExpression (keyIndex -> k) sym =
  fmap (fmap Mapnik.Expression) $ newTextMaybe $ \(ptr, len) ->
    [C.block|void {
    boost::optional<expression_ptr> expr = get_optional<expression_ptr>(*$(symbolizer_base *sym), $(keys k));
    if (expr && *expr) {
      std::string s = to_expression_string(**expr);
      *$(char** ptr) = strdup(s.c_str());
      *$(int* len) = s.length();
    } else {
      *$(char** ptr) = NULL;
    }
    }|]


keyIndex :: Key a -> C.CUChar
keyIndex Gamma = [C.pure|keys{keys::gamma}|]
keyIndex GammaMethod = [C.pure|keys{keys::gamma_method}|]
keyIndex Opacity = [C.pure|keys{keys::opacity}|]
keyIndex Alignment = [C.pure|keys{keys::alignment}|]
keyIndex Offset = [C.pure|keys{keys::offset}|]
keyIndex CompOp = [C.pure|keys{keys::comp_op}|]
keyIndex Clip = [C.pure|keys{keys::clip}|]
keyIndex Fill = [C.pure|keys{keys::fill}|]
keyIndex FillOpacity = [C.pure|keys{keys::fill_opacity}|]
keyIndex Stroke = [C.pure|keys{keys::stroke}|]
keyIndex StrokeWidth = [C.pure|keys{keys::stroke_width}|]
keyIndex StrokeOpacity = [C.pure|keys{keys::stroke_opacity}|]
keyIndex StrokeLinejoin = [C.pure|keys{keys::stroke_linejoin}|]
keyIndex StrokeLinecap = [C.pure|keys{keys::stroke_linecap}|]
keyIndex StrokeGamma = [C.pure|keys{keys::stroke_gamma}|]
keyIndex StrokeGammaMethod = [C.pure|keys{keys::stroke_gamma_method}|]
keyIndex StrokeDashoffset = [C.pure|keys{keys::stroke_dashoffset}|]
keyIndex StrokeDasharray = [C.pure|keys{keys::stroke_dasharray}|]
keyIndex StrokeMiterlimit = [C.pure|keys{keys::stroke_miterlimit}|]
keyIndex GeometryTransform = [C.pure|keys{keys::geometry_transform}|]
keyIndex LineRasterizer = [C.pure|keys{keys::line_rasterizer}|]
keyIndex ImageTransform = [C.pure|keys{keys::image_transform}|]
keyIndex Spacing = [C.pure|keys{keys::spacing}|]
keyIndex MaxError = [C.pure|keys{keys::max_error}|]
keyIndex AllowOverlap = [C.pure|keys{keys::allow_overlap}|]
keyIndex IgnorePlacement = [C.pure|keys{keys::ignore_placement}|]
keyIndex Width = [C.pure|keys{keys::width}|]
keyIndex Height = [C.pure|keys{keys::height}|]
keyIndex File = [C.pure|keys{keys::file}|]
keyIndex ShieldDx = [C.pure|keys{keys::shield_dx}|]
keyIndex ShieldDy = [C.pure|keys{keys::shield_dy}|]
keyIndex UnlockImage = [C.pure|keys{keys::unlock_image}|]
keyIndex Mode = [C.pure|keys{keys::mode}|]
keyIndex Scaling = [C.pure|keys{keys::scaling}|]
keyIndex FilterFactor = [C.pure|keys{keys::filter_factor}|]
keyIndex MeshSize = [C.pure|keys{keys::mesh_size}|]
keyIndex Premultiplied = [C.pure|keys{keys::premultiplied}|]
keyIndex Smooth = [C.pure|keys{keys::smooth}|]
keyIndex SimplifyAlgorithm = [C.pure|keys{keys::simplify_algorithm}|]
keyIndex SimplifyTolerance = [C.pure|keys{keys::simplify_tolerance}|]
keyIndex HaloRasterizer = [C.pure|keys{keys::halo_rasterizer}|]
keyIndex TextPlacements = [C.pure|keys{keys::text_placements_}|]
keyIndex LabelPlacement = [C.pure|keys{keys::label_placement}|]
keyIndex MarkersPlacementType = [C.pure|keys{keys::markers_placement_type}|]
keyIndex MarkersMultipolicy = [C.pure|keys{keys::markers_multipolicy}|]
keyIndex PointPlacementType = [C.pure|keys{keys::point_placement_type}|]
keyIndex Colorizer = [C.pure|keys{keys::colorizer}|]
keyIndex HaloTransform = [C.pure|keys{keys::halo_transform}|]
keyIndex NumColumns = [C.pure|keys{keys::num_columns}|]
keyIndex StartColumn = [C.pure|keys{keys::start_column}|]
keyIndex RepeatKey = [C.pure|keys{keys::repeat_key}|]
keyIndex GroupProperties = [C.pure|keys{keys::group_properties}|]
keyIndex LargestBoxOnly = [C.pure|keys{keys::largest_box_only}|]
keyIndex MinimumPathLength = [C.pure|keys{keys::minimum_path_length}|]
keyIndex HaloCompOp = [C.pure|keys{keys::halo_comp_op}|]
keyIndex TextTransform = [C.pure|keys{keys::text_transform}|]
keyIndex HorizontalAlignment = [C.pure|keys{keys::horizontal_alignment}|]
keyIndex JustifyAlignment = [C.pure|keys{keys::justify_alignment}|]
keyIndex VerticalAlignment = [C.pure|keys{keys::vertical_alignment}|]
keyIndex Upright = [C.pure|keys{keys::upright}|]
keyIndex Direction = [C.pure|keys{keys::direction}|]
keyIndex AvoidEdges = [C.pure|keys{keys::avoid_edges}|]
keyIndex FfSettings = [C.pure|keys{keys::ff_settings}|]
