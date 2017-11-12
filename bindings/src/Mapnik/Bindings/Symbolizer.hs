{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
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
  create
, unCreate
, unsafeNew
) where

import           Mapnik.Lens
import qualified Mapnik
import           Mapnik.Enums
import           Mapnik ( Transform(..), Prop (..) )
import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Orphans ()
import           Mapnik.Bindings.SymbolizerValue (SymValue(..))
import           Mapnik.Bindings.TextPlacements ()

import           Data.IORef
import           Control.Exception
import           Control.Lens hiding (has)
import           Data.Maybe (catMaybes)
import           Data.Text (Text, unpack)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/symbolizer.hpp>"
C.include "<mapnik/symbolizer_base.hpp>"
C.include "<mapnik/symbolizer_utils.hpp>"
C.include "<mapnik/symbolizer_keys.hpp>"
C.include "<mapnik/expression_string.hpp>"
C.include "<mapnik/expression_evaluator.hpp>"
C.include "<mapnik/text/font_feature_settings.hpp>"
C.include "symbolizer_util.hpp"

C.using "namespace mapnik"
C.verbatim "typedef symbolizer_base::value_type sym_value_type;"

--
-- * Symbolizer


foreign import ccall "&hs_mapnik_destroy_Symbolizer" destroySymbolizer :: FinalizerPtr Symbolizer

unsafeNew :: Ptr Symbolizer -> IO Symbolizer
unsafeNew = fmap Symbolizer . newForeignPtr destroySymbolizer

create :: Mapnik.Symbolizer -> IO Symbolizer
create sym = bracket alloc dealloc $ \p -> do
  mapM_ (`setProperty` p) (sym^.symbolizerProps)
  unsafeNew =<< castSym sym p
  where
    alloc = [C.exp|symbolizer_base * { new symbolizer_base() }|]
    dealloc p = [C.block|void { delete $(symbolizer_base *p);}|]

unCreate :: Symbolizer -> IO Mapnik.Symbolizer
unCreate sym = do
  symName <- getName sym
  case defFromName symName of
    Just s -> do
      props <- getProperties sym
      return (s & symbolizerProps .~ props)
    Nothing -> throwIO (userError ("Unexpected symbolizer name: " ++ unpack symName))

getProperties :: Symbolizer -> IO [Property]
getProperties sym' = bracket alloc dealloc $ \sym -> do
  ret <- newIORef []
  let cb :: Ptr SymbolizerValue -> C.CUChar -> IO ()
      cb p = withKey $ \k -> do
               v <- peekSv p
               case v of
                 Just prop -> modifyIORef ret ((k :=> prop):)
                 Nothing   -> throwIO (userError "Unexpected type")
  [C.block|void {
    symbolizer_base::cont_type const& props =
      $(symbolizer_base *sym)->properties;
    for (auto it=props.begin(); it!=props.end(); ++it) {
      $fun:(void (*cb)(sym_value_type *, unsigned char))(const_cast<sym_value_type*>(&(it->second)),
                                                         static_cast<unsigned char>(it->first));
    }
    }|]
  readIORef ret
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


data Property where
  (:=>) :: SymValue v => Key v -> Prop v -> Property

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
    Mode :: Key DebugMode
    Scaling :: Key ScalingMethod
    FilterFactor :: Key Double
    MeshSize :: Key Int
    Premultiplied :: Key Bool
    Smooth :: Key Double
    SimplifyAlgorithm :: Key SimplifyAlgorithm
    SimplifyTolerance :: Key Double
    HaloRasterizer :: Key HaloRasterizer
    TextPlacementsKey :: Key Mapnik.TextPlacements
    LabelPlacement :: Key LabelPlacement
    MarkersPlacementKey :: Key MarkerPlacement
    MarkersMultipolicy :: Key MarkerMultiPolicy
    PointPlacementKey :: Key PointPlacement
    ColorizerKey :: Key Mapnik.Colorizer
    HaloTransform :: Key Mapnik.Transform
    NumColumns :: Key Int
    StartColumn :: Key Int
    RepeatKey :: Key Mapnik.Expression
    GroupPropertiesKey :: Key Mapnik.GroupProperties
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
    FfSettings :: Key Mapnik.FontFeatureSettings


symbolizerProps :: Lens' Mapnik.Symbolizer Properties
symbolizerProps = lens getProps setProps where
  setProps sym@Mapnik.Point{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (File               :=> v) = file ?~ v
    step (Opacity            :=> v) = opacity ?~ v
    step (AllowOverlap       :=> v) = allowOverlap ?~ v
    step (IgnorePlacement    :=> v) = ignorePlacement ?~ v
    step (PointPlacementKey :=> v) = pointPlacement ?~ v
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
    step (Scaling       :=> Val v        ) = scaling ?~ v
    step (Opacity       :=> Val v        ) = rasterOpacity ?~ v
    step (FilterFactor  :=> Val v        ) = filterFactor ?~ v
    step (MeshSize      :=> Val v        ) = meshSize ?~ v
    step (Premultiplied :=> Val v        ) = preMultiplied ?~ v
    step (ColorizerKey :=> Val v        ) = colorizer ?~ v
    step p                                 = stepBase p

  setProps sym@Mapnik.Shield{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (TextPlacementsKey :=> Val v) = placements ?~ v
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
    step (TextPlacementsKey :=> Val v) = placements ?~ v
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
    step (MarkersPlacementKey :=> v) = placement ?~ v
    step (MarkersMultipolicy   :=> v) = multiPolicy ?~ v
    step (Direction            :=> v) = direction ?~ v
    step p                            = stepStroke p

  setProps sym@Mapnik.Group{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (GroupPropertiesKey :=> v) = groupProperties ?~ v
    step (NumColumns      :=> v) = numColumns ?~ v
    step (StartColumn     :=> v) = startColumn ?~ v
    step (RepeatKey       :=> v) = repeatKey ?~ v
    step (TextPlacementsKey :=> Val v) = placements ?~ v
    step p                       = stepBase p

  setProps sym@Mapnik.Debug{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (Mode :=> Val v) = mode ?~ v
    step p                = stepBase p

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
      , fmap (PointPlacementKey :=>) (sym^?!pointPlacement)
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
      [ fmap ((Scaling       :=>) . Val) (sym^?!scaling)
      , fmap ((Opacity       :=>) . Val) (sym^?!rasterOpacity)
      , fmap ((FilterFactor  :=>) . Val) (sym^?!filterFactor)
      , fmap ((MeshSize      :=>) . Val) (sym^?!meshSize)
      , fmap ((Premultiplied :=>) . Val) (sym^?!preMultiplied)
      , fmap ((ColorizerKey :=>) . Val) (sym^?!colorizer)
      , GET_BASE_PROPS
      ]
    Mapnik.Shield{} ->
      [ fmap ((TextPlacementsKey :=>) . Val) (sym^?!placements)
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
      [ fmap ((TextPlacementsKey :=>) . Val) (sym^?!placements)
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
      , fmap (MarkersPlacementKey :=>) (sym^?!placement)
      , fmap (MarkersMultipolicy   :=>) (sym^?!multiPolicy)
      , fmap (Direction            :=>) (sym^?!direction)
      , GET_STROKE_PROPS
      ]
    Mapnik.Group{} ->
      [ fmap (GroupPropertiesKey :=>) (sym^?!groupProperties)
      , fmap (NumColumns      :=>) (sym^?!numColumns)
      , fmap (StartColumn     :=>) (sym^?!startColumn)
      , fmap (RepeatKey       :=>) (sym^?!repeatKey)
      , fmap ((TextPlacementsKey :=>) . Val) (sym^?!placements)
      , GET_BASE_PROPS
      ]
    Mapnik.Debug{} ->
      [ fmap ((Mode :=>) . Val)  (sym^?!mode)
      , GET_BASE_PROPS
      ]
    Mapnik.Dot{} ->
      [ fmap (Fill    :=>) (sym^?!fill)
      , fmap (Opacity :=>) (sym^?!opacity)
      , fmap (Width   :=>) (sym^?!width)
      , fmap (Height  :=>) (sym^?!height)
      , fmap (CompOp  :=>) (sym^?!compOp)
      ]

setProperty :: Property -> Ptr SymbolizerBase -> IO ()
setProperty ((keyIndex -> k) :=> (flip pokeSv -> cb)) sym =
  [C.block|void {
    sym_value_type val;
    $fun:(void (*cb)(sym_value_type*))(&val);
    $(symbolizer_base *sym)->properties[$(keys k)] = val;
  }|]

withKey :: (forall a. SymValue a => Key a -> IO b) -> C.CUChar -> IO b
withKey f = \k -> if
  | k == [C.pure|keys{keys::gamma}|] -> f Gamma
  | k == [C.pure|keys{keys::gamma_method}|] -> f GammaMethod
  | k == [C.pure|keys{keys::opacity}|] -> f Opacity
  | k == [C.pure|keys{keys::alignment}|] -> f Alignment
  | k == [C.pure|keys{keys::offset}|] -> f Offset
  | k == [C.pure|keys{keys::comp_op}|] -> f CompOp
  | k == [C.pure|keys{keys::clip}|] -> f Clip
  | k == [C.pure|keys{keys::fill}|] -> f Fill
  | k == [C.pure|keys{keys::fill_opacity}|] -> f FillOpacity
  | k == [C.pure|keys{keys::stroke}|] -> f Stroke
  | k == [C.pure|keys{keys::stroke_width}|] -> f StrokeWidth
  | k == [C.pure|keys{keys::stroke_opacity}|] -> f StrokeOpacity
  | k == [C.pure|keys{keys::stroke_linejoin}|] -> f StrokeLinejoin
  | k == [C.pure|keys{keys::stroke_linecap}|] -> f StrokeLinecap
  | k == [C.pure|keys{keys::stroke_gamma}|] -> f StrokeGamma
  | k == [C.pure|keys{keys::stroke_gamma_method}|] -> f StrokeGammaMethod
  | k == [C.pure|keys{keys::stroke_dashoffset}|] -> f StrokeDashoffset
  | k == [C.pure|keys{keys::stroke_dasharray}|] -> f StrokeDasharray
  | k == [C.pure|keys{keys::stroke_miterlimit}|] -> f StrokeMiterlimit
  | k == [C.pure|keys{keys::geometry_transform}|] -> f GeometryTransform
  | k == [C.pure|keys{keys::line_rasterizer}|] -> f LineRasterizer
  | k == [C.pure|keys{keys::image_transform}|] -> f ImageTransform
  | k == [C.pure|keys{keys::spacing}|] -> f Spacing
  | k == [C.pure|keys{keys::max_error}|] -> f MaxError
  | k == [C.pure|keys{keys::allow_overlap}|] -> f AllowOverlap
  | k == [C.pure|keys{keys::ignore_placement}|] -> f IgnorePlacement
  | k == [C.pure|keys{keys::width}|] -> f Width
  | k == [C.pure|keys{keys::height}|] -> f Height
  | k == [C.pure|keys{keys::file}|] -> f File
  | k == [C.pure|keys{keys::shield_dx}|] -> f ShieldDx
  | k == [C.pure|keys{keys::shield_dy}|] -> f ShieldDy
  | k == [C.pure|keys{keys::unlock_image}|] -> f UnlockImage
  | k == [C.pure|keys{keys::mode}|] -> f Mode
  | k == [C.pure|keys{keys::scaling}|] -> f Scaling
  | k == [C.pure|keys{keys::filter_factor}|] -> f FilterFactor
  | k == [C.pure|keys{keys::mesh_size}|] -> f MeshSize
  | k == [C.pure|keys{keys::premultiplied}|] -> f Premultiplied
  | k == [C.pure|keys{keys::smooth}|] -> f Smooth
  | k == [C.pure|keys{keys::simplify_algorithm}|] -> f SimplifyAlgorithm
  | k == [C.pure|keys{keys::simplify_tolerance}|] -> f SimplifyTolerance
  | k == [C.pure|keys{keys::halo_rasterizer}|] -> f HaloRasterizer
  | k == [C.pure|keys{keys::text_placements_}|] -> f TextPlacementsKey
  | k == [C.pure|keys{keys::label_placement}|] -> f LabelPlacement
  | k == [C.pure|keys{keys::markers_placement_type}|] -> f MarkersPlacementKey
  | k == [C.pure|keys{keys::markers_multipolicy}|] -> f MarkersMultipolicy
  | k == [C.pure|keys{keys::point_placement_type}|] -> f PointPlacementKey
  | k == [C.pure|keys{keys::colorizer}|] -> f ColorizerKey
  | k == [C.pure|keys{keys::halo_transform}|] -> f HaloTransform
  | k == [C.pure|keys{keys::num_columns}|] -> f NumColumns
  | k == [C.pure|keys{keys::start_column}|] -> f StartColumn
  | k == [C.pure|keys{keys::repeat_key}|] -> f RepeatKey
  | k == [C.pure|keys{keys::group_properties}|] -> f GroupPropertiesKey
  | k == [C.pure|keys{keys::largest_box_only}|] -> f LargestBoxOnly
  | k == [C.pure|keys{keys::minimum_path_length}|] -> f MinimumPathLength
  | k == [C.pure|keys{keys::halo_comp_op}|] -> f HaloCompOp
  | k == [C.pure|keys{keys::text_transform}|] -> f TextTransform
  | k == [C.pure|keys{keys::horizontal_alignment}|] -> f HorizontalAlignment
  | k == [C.pure|keys{keys::justify_alignment}|] -> f JustifyAlignment
  | k == [C.pure|keys{keys::vertical_alignment}|] -> f VerticalAlignment
  | k == [C.pure|keys{keys::upright}|] -> f Upright
  | k == [C.pure|keys{keys::direction}|] -> f Direction
  | k == [C.pure|keys{keys::avoid_edges}|] -> f AvoidEdges
  | k == [C.pure|keys{keys::ff_settings}|] -> f FfSettings

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
keyIndex TextPlacementsKey = [C.pure|keys{keys::text_placements_}|]
keyIndex LabelPlacement = [C.pure|keys{keys::label_placement}|]
keyIndex MarkersPlacementKey = [C.pure|keys{keys::markers_placement_type}|]
keyIndex MarkersMultipolicy = [C.pure|keys{keys::markers_multipolicy}|]
keyIndex PointPlacementKey = [C.pure|keys{keys::point_placement_type}|]
keyIndex ColorizerKey = [C.pure|keys{keys::colorizer}|]
keyIndex HaloTransform = [C.pure|keys{keys::halo_transform}|]
keyIndex NumColumns = [C.pure|keys{keys::num_columns}|]
keyIndex StartColumn = [C.pure|keys{keys::start_column}|]
keyIndex RepeatKey = [C.pure|keys{keys::repeat_key}|]
keyIndex GroupPropertiesKey = [C.pure|keys{keys::group_properties}|]
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
