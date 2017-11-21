{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Mapnik.Bindings.Symbolizer (
  create
, unCreate
, unsafeNew
) where

import qualified Mapnik
import           Mapnik.Lens
import           Mapnik.Symbolizer (Prop(..))
import qualified Mapnik.Symbolizer as Mapnik
import qualified Mapnik.Common as Mapnik
import           Mapnik.Enums
import qualified Mapnik.Bindings.Expression as Expression
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Colorizer
import           Mapnik.Bindings.Util
import qualified Mapnik.Bindings.Transform as Transform
import           Mapnik.Bindings.Orphans ()
import           Mapnik.Bindings.Variant
import qualified Mapnik.Bindings.Cpp as C

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Lens hiding (has)
import           Control.Monad
import           Control.Monad.Base (MonadBase, liftBase)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Reader
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text (Text, pack, unpack)
import           Data.IORef.Lifted
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Storable         as V
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Marshal.Alloc (finalizerFree)
import           Foreign.Marshal.Utils.Lifted (with)
import           Foreign.Ptr (Ptr, castPtr, nullPtr)
import           Foreign.Storable (poke)



C.context mapnikCtx

C.include "<string>"
C.include "<cassert>"
C.include "<mapnik/util/variant.hpp>"
C.include "<mapnik/symbolizer.hpp>"
C.include "<mapnik/symbolizer_base.hpp>"
C.include "<mapnik/symbolizer_utils.hpp>"
C.include "<mapnik/symbolizer_keys.hpp>"
C.include "<mapnik/expression_string.hpp>"
C.include "<mapnik/expression_evaluator.hpp>"
C.include "<mapnik/transform_processor.hpp>"
C.include "<mapnik/text/font_feature_settings.hpp>"
C.include "<mapnik/text/placements/base.hpp>"
C.include "<mapnik/text/placements/dummy.hpp>"
C.include "<mapnik/text/placements/simple.hpp>"
C.include "<mapnik/text/placements/list.hpp>"
C.include "<mapnik/text/text_properties.hpp>"
C.include "<mapnik/text/formatting/base.hpp>"
C.include "<mapnik/text/formatting/text.hpp>"
C.include "<mapnik/text/formatting/list.hpp>"
C.include "<mapnik/text/formatting/format.hpp>"
C.include "<mapnik/text/formatting/layout.hpp>"
C.include "<mapnik/group/group_rule.hpp>"

C.include "symbolizer_util.hpp"
C.include "util.hpp"

C.using "namespace mapnik"
C.using "namespace mapnik::formatting"
C.using "sym_value_type = symbolizer_base::value_type"
C.using "dash_t = std::pair<double,double>"


--
-- * Symbolizer


foreign import ccall "&hs_mapnik_destroy_Symbolizer" destroySymbolizer :: FinalizerPtr Symbolizer

unsafeNew :: MonadBase IO m => Ptr Symbolizer -> m Symbolizer
unsafeNew = fmap Symbolizer . liftBase . newForeignPtr destroySymbolizer

create :: Mapnik.Symbolizer -> IO Symbolizer
create = flip runReaderT undefined . create'

type instance VariantM SymbolizerValue = ReaderT Mapnik.FontSetMap IO
type SvM = VariantM SymbolizerValue

create' :: Mapnik.Symbolizer -> SvM Symbolizer
create' sym = bracket alloc dealloc $ \p -> do
  mapM_ (`setProperty` p) (sym^.symbolizerProps)
  unsafeNew =<< toSym p
  where
    dealloc p = [C.block|void { delete $(symbolizer_base *p);}|]
    alloc = case sym of
      Mapnik.PointSymbolizer{} ->
        [C.exp|symbolizer_base *{ new point_symbolizer}|]
      Mapnik.LineSymbolizer{} ->
        [C.exp|symbolizer_base *{ new line_symbolizer}|]
      Mapnik.LinePatternSymbolizer{} ->
        [C.exp|symbolizer_base *{ new line_pattern_symbolizer}|]
      Mapnik.PolygonSymbolizer{} ->
        [C.exp|symbolizer_base *{ new polygon_symbolizer}|]
      Mapnik.PolygonPatternSymbolizer{} ->
        [C.exp|symbolizer_base *{ new polygon_pattern_symbolizer}|]
      Mapnik.RasterSymbolizer{} ->
        [C.exp|symbolizer_base *{ new raster_symbolizer}|]
      Mapnik.ShieldSymbolizer{} ->
        [C.exp|symbolizer_base *{ new shield_symbolizer}|]
      Mapnik.TextSymbolizer{} ->
        [C.exp|symbolizer_base *{ new text_symbolizer}|]
      Mapnik.BuildingSymbolizer{} ->
        [C.exp|symbolizer_base *{ new building_symbolizer}|]
      Mapnik.MarkersSymbolizer{} ->
        [C.exp|symbolizer_base *{ new markers_symbolizer}|]
      Mapnik.GroupSymbolizer{} ->
        [C.exp|symbolizer_base *{ new group_symbolizer}|]
      Mapnik.DebugSymbolizer{} ->
        [C.exp|symbolizer_base *{ new debug_symbolizer}|]
      Mapnik.DotSymbolizer{} ->
        [C.exp|symbolizer_base *{ new dot_symbolizer}|]

    toSym p = case sym of
      Mapnik.PointSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<point_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.LineSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<line_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.LinePatternSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<line_pattern_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.PolygonSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<polygon_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.PolygonPatternSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<polygon_pattern_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.RasterSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<raster_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.ShieldSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<shield_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.TextSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<text_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.BuildingSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<building_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.MarkersSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<markers_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.GroupSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<group_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.DebugSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<debug_symbolizer*>($(symbolizer_base *p)))}|]
      Mapnik.DotSymbolizer{} ->
        [C.exp|symbolizer *{ new symbolizer(*static_cast<dot_symbolizer*>($(symbolizer_base *p)))}|]

unCreate :: Symbolizer -> IO Mapnik.Symbolizer
unCreate sym = runReaderT (unCreate' sym) mempty

unCreate' :: Symbolizer -> SvM Mapnik.Symbolizer
unCreate' sym = do
  symName <- getName sym
  case defFromName symName of
    Just s -> do
      props <- getProperties sym
      return (s & symbolizerProps .~ props)
    Nothing -> throwIO (userError ("Unexpected symbolizer name: " ++ unpack symName))

getProperties :: Symbolizer -> SvM [Property]
getProperties sym' = bracket alloc dealloc $ \sym -> do
  ret <- newIORef []
  env <- ask
  let cb :: Ptr SymbolizerValue -> C.CUChar -> IO ()
      cb p = withKey $ \(k :: Key a) -> do prop <- runReaderT (peekV p) env
                                           modifyIORef' ret ((k :=> prop):)
  [C.safeBlock|void {
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
    dealloc p = [C.exp|void { delete $(symbolizer_base *p)}|]

getName :: MonadBaseControl IO m => Symbolizer -> m Text
getName s = newText "Symbolizer.getName" $ \(p, len) ->
  [C.block|void {
  std::string s = symbolizer_name(*$fptr-ptr:(symbolizer *s));
  mallocedString(s, $(char **p), $(int *len));
  }|]

defFromName :: Text -> Maybe Mapnik.Symbolizer
defFromName "PointSymbolizer"          = Just Mapnik.pointSym
defFromName "LineSymbolizer"           = Just Mapnik.lineSym
defFromName "LinePatternSymbolizer"    = Just Mapnik.linePatternSym
defFromName "PolygonSymbolizer"        = Just Mapnik.polygonSym
defFromName "PolygonPatternSymbolizer" = Just Mapnik.polygonPatternSym
defFromName "RasterSymbolizer"         = Just Mapnik.rasterSym
defFromName "ShieldSymbolizer"         = Just Mapnik.shieldSym
defFromName "TextSymbolizer"           = Just Mapnik.textSym
defFromName "BuildingSymbolizer"       = Just Mapnik.buildingSym
defFromName "MarkersSymbolizer"        = Just Mapnik.markersSym
defFromName "GroupSymbolizer"          = Just Mapnik.groupSym
defFromName "DebugSymbolizer"          = Just Mapnik.debugSym
defFromName "DotSymbolizer"            = Just Mapnik.dotSym
defFromName _                          = Nothing


data Property where
  (:=>) :: Variant SymbolizerValue v => Key v -> Prop v -> Property

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
    GroupPropertiesKey :: Key Mapnik.GroupSymProperties
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

deriving instance Show (Key a)

symbolizerProps :: Lens' Mapnik.Symbolizer Properties
symbolizerProps = lens getProps setProps where
  setProps sym@Mapnik.PointSymbolizer{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (File               :=> v) = file ?~ v
    step (Opacity            :=> v) = opacity ?~ v
    step (AllowOverlap       :=> v) = allowOverlap ?~ v
    step (IgnorePlacement    :=> v) = ignorePlacement ?~ v
    step (PointPlacementKey :=> v) = pointPlacement ?~ v
    step (ImageTransform     :=> v) = imageTransform ?~ v
    step p                          = stepBase p

  setProps sym@Mapnik.LineSymbolizer{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (Opacity :=> v) = opacity ?~ v
    step (Offset  :=> v) = offset ?~ v
    step (LineRasterizer :=> v) = lineRasterizer ?~ v
    step p               = stepStroke p

  setProps sym@Mapnik.LinePatternSymbolizer{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (File            :=> v) = file ?~ v
    step (Opacity         :=> v) = opacity ?~ v
    step (Offset          :=> v) = offset ?~ v
    step (ImageTransform  :=> v) = imageTransform ?~ v
    step p                       = stepBase p

  setProps sym@Mapnik.PolygonSymbolizer{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (Fill        :=> v) = fill ?~ v
    step (FillOpacity :=> v) = fillOpacity ?~ v
    step (Gamma       :=> v) = gamma ?~ v
    step (GammaMethod :=> v) = gammaMethod ?~ v
    step p                   = stepBase p

  setProps sym@Mapnik.PolygonPatternSymbolizer{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (File           :=> v) = file ?~ v
    step (Opacity        :=> v) = opacity ?~ v
    step (Gamma          :=> v) = gamma ?~ v
    step (GammaMethod    :=> v) = gammaMethod ?~ v
    step (ImageTransform :=> v) = imageTransform ?~ v
    step (Alignment      :=> v) = alignment ?~ v
    step p                      = stepBase p

  setProps sym@Mapnik.RasterSymbolizer{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (Scaling       :=> Val v        ) = scaling ?~ v
    step (Opacity       :=> Val v        ) = rasterOpacity ?~ v
    step (FilterFactor  :=> Val v        ) = filterFactor ?~ v
    step (MeshSize      :=> Val v        ) = meshSize ?~ v
    step (Premultiplied :=> Val v        ) = preMultiplied ?~ v
    step (ColorizerKey :=> Val v        ) = colorizer ?~ v
    step p                                 = stepBase p

  setProps sym@Mapnik.ShieldSymbolizer{} = foldr step sym  where
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

  setProps sym@Mapnik.TextSymbolizer{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (TextPlacementsKey :=> Val v) = placements ?~ v
    step (HaloCompOp     :=> v) = haloCompOp ?~ v
    step (HaloRasterizer :=> v) = haloRasterizer ?~ v
    step (HaloTransform  :=> v) = haloTransform ?~ v
    step p                      = stepBase p

  setProps sym@Mapnik.BuildingSymbolizer{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (Fill        :=> v) = fill ?~ v
    step (FillOpacity :=> v) = fillOpacity ?~ v
    step (Height      :=> v) = height ?~ v
    step p                   = stepBase p

  setProps sym@Mapnik.MarkersSymbolizer{} = foldr step sym  where
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

  setProps sym@Mapnik.GroupSymbolizer{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (GroupPropertiesKey :=> v) = groupProperties ?~ v
    step (NumColumns      :=> v) = numColumns ?~ v
    step (StartColumn     :=> v) = startColumn ?~ v
    step (RepeatKey       :=> v) = repeatKey ?~ v
    step (TextPlacementsKey :=> Val v) = placements ?~ v
    step p                       = stepBase p

  setProps sym@Mapnik.DebugSymbolizer{} = foldr step sym  where
    step :: Property -> Mapnik.Symbolizer -> Mapnik.Symbolizer
    step (Mode :=> Val v) = mode ?~ v
    step p                = stepBase p

  setProps sym@Mapnik.DotSymbolizer{} = foldr step sym  where
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
    Mapnik.PointSymbolizer{} ->
      [ fmap (File               :=>) (sym^?!file)
      , fmap (Opacity            :=>) (sym^?!opacity)
      , fmap (AllowOverlap       :=>) (sym^?!allowOverlap)
      , fmap (IgnorePlacement    :=>) (sym^?!ignorePlacement)
      , fmap (PointPlacementKey :=>) (sym^?!pointPlacement)
      , fmap (ImageTransform     :=>) (sym^?!imageTransform)
      , GET_BASE_PROPS
      ]
    Mapnik.LineSymbolizer{} ->
      [ fmap (Offset         :=>) (sym^?!offset)
      , fmap (LineRasterizer :=>) (sym^?!lineRasterizer)
      , GET_STROKE_PROPS
      ]
    Mapnik.LinePatternSymbolizer{} ->
      [ fmap (File           :=>) (sym^?!file)
      , fmap (Opacity        :=>) (sym^?!opacity)
      , fmap (Offset         :=>) (sym^?!offset)
      , fmap (ImageTransform :=>) (sym^?!imageTransform)
      , GET_BASE_PROPS
      ]
    Mapnik.PolygonSymbolizer{} ->
      [ fmap (Fill        :=>) (sym^?!fill)
      , fmap (FillOpacity :=>) (sym^?!fillOpacity)
      , fmap (Gamma       :=>) (sym^?!gamma)
      , fmap (GammaMethod :=>) (sym^?!gammaMethod)
      , GET_BASE_PROPS
      ]
    Mapnik.PolygonPatternSymbolizer{} ->
      [ fmap (File           :=>) (sym^?!file)
      , fmap (Opacity        :=>) (sym^?!opacity)
      , fmap (Gamma          :=>) (sym^?!gamma)
      , fmap (GammaMethod    :=>) (sym^?!gammaMethod)
      , fmap (ImageTransform :=>) (sym^?!imageTransform)
      , fmap (Alignment      :=>) (sym^?!alignment)
      , GET_BASE_PROPS
      ]
    Mapnik.RasterSymbolizer{} ->
      [ fmap ((Scaling       :=>) . Val) (sym^?!scaling)
      , fmap ((Opacity       :=>) . Val) (sym^?!rasterOpacity)
      , fmap ((FilterFactor  :=>) . Val) (sym^?!filterFactor)
      , fmap ((MeshSize      :=>) . Val) (sym^?!meshSize)
      , fmap ((Premultiplied :=>) . Val) (sym^?!preMultiplied)
      , fmap ((ColorizerKey :=>) . Val) (sym^?!colorizer)
      , GET_BASE_PROPS
      ]
    Mapnik.ShieldSymbolizer{} ->
      [ fmap ((TextPlacementsKey :=>) . Val) (sym^?!placements)
      , fmap (ImageTransform :=>) (sym^?!imageTransform)
      , fmap (ShieldDx          :=>) (sym^?!dx)
      , fmap (ShieldDy          :=>) (sym^?!dy)
      , fmap (Opacity           :=>) (sym^?!opacity)
      , fmap (UnlockImage       :=>) (sym^?!unlockImage)
      , fmap (File              :=>) (sym^?!file)
      , fmap (HaloRasterizer    :=>) (sym^?!haloRasterizer)
      , GET_BASE_PROPS
      ]
    Mapnik.TextSymbolizer{} ->
      [ fmap ((TextPlacementsKey :=>) . Val) (sym^?!placements)
      , fmap (HaloCompOp        :=>) (sym^?!haloCompOp)
      , fmap (HaloRasterizer    :=>) (sym^?!haloRasterizer)
      , fmap (HaloTransform :=>) (sym^?!haloTransform)
      , GET_BASE_PROPS
      ]
    Mapnik.BuildingSymbolizer{} ->
      [ fmap (Fill        :=>) (sym^?!fill)
      , fmap (FillOpacity :=>) (sym^?!fillOpacity)
      , fmap (Height      :=>) (sym^?!height)
      , GET_BASE_PROPS
      ]
    Mapnik.MarkersSymbolizer{} ->
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
      , fmap (ImageTransform       :=>) (sym^?!imageTransform)
      , fmap (MarkersPlacementKey :=>) (sym^?!placement)
      , fmap (MarkersMultipolicy   :=>) (sym^?!multiPolicy)
      , fmap (Direction            :=>) (sym^?!direction)
      , GET_STROKE_PROPS
      ]
    Mapnik.GroupSymbolizer{} ->
      [ fmap (GroupPropertiesKey :=>) (sym^?!groupProperties)
      , fmap (NumColumns      :=>) (sym^?!numColumns)
      , fmap (StartColumn     :=>) (sym^?!startColumn)
      , fmap (RepeatKey       :=>) (sym^?!repeatKey)
      , fmap ((TextPlacementsKey :=>) . Val) (sym^?!placements)
      , GET_BASE_PROPS
      ]
    Mapnik.DebugSymbolizer{} ->
      [ fmap ((Mode :=>) . Val)  (sym^?!mode)
      , GET_BASE_PROPS
      ]
    Mapnik.DotSymbolizer{} ->
      [ fmap (Fill    :=>) (sym^?!fill)
      , fmap (Opacity :=>) (sym^?!opacity)
      , fmap (Width   :=>) (sym^?!width)
      , fmap (Height  :=>) (sym^?!height)
      , fmap (CompOp  :=>) (sym^?!compOp)
      ]

setProperty :: Property -> Ptr SymbolizerBase -> SvM ()
setProperty ((keyIndex -> k) :=> v) sym = do
  env <- ask
  let cb p = runReaderT (pokeV p v) env
  [C.safeBlock|void {
    sym_value_type val;
    $fun:(void (*cb)(sym_value_type*))(&val);
    $(symbolizer_base *sym)->properties[$(keys k)] = val;
  }|]

withKey
  :: MonadBaseControl IO m
  => (forall a. Variant SymbolizerValue a => Key a -> m b)
  -> C.CUChar -> m b
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




------------------------------------------------------------------------------------------
-- * TextPlacements
------------------------------------------------------------------------------------------


foreign import ccall "&hs_mapnik_destroy_TextPlacements" destroyTextPlacements :: FinalizerPtr TextPlacements

createTextPlacements :: Mapnik.TextPlacements -> SvM TextPlacements
createTextPlacements (Mapnik.Dummy defs) = unsafeNew $ \p -> do
  defaults <- createTextSymProps defs
  [C.block|void {
    auto placements = std::make_shared<text_placements_dummy>();
    *$(text_placements_ptr **p) = new text_placements_ptr(placements);
    placements->defaults = *$fptr-ptr:(text_symbolizer_properties *defaults);
  }|]
  where
    unsafeNew = mkUnsafeNew TextPlacements destroyTextPlacements



unCreateTextPlacements :: TextPlacements -> SvM Mapnik.TextPlacements
unCreateTextPlacements p =
  fmap Mapnik.Dummy . unCreateTextSymProps =<< C.withPtr_ (\ret ->
    [C.block|void {
    auto ptr = dynamic_cast<text_placements_dummy *>($fptr-ptr:(text_placements_ptr *p)->get());
    assert(ptr);
    *$(text_symbolizer_properties **ret) = &ptr->defaults;
    }|])


instance Variant SymbolizerValue Mapnik.TextPlacements where
  peekV p = unCreateTextPlacements =<<
    justOrTypeError "TextPlacements" (unsafeNewMaybe $ \ret ->
      [C.block|void {
      try {
        *$(text_placements_ptr **ret) =
          new text_placements_ptr(util::get<text_placements_ptr>(*$(sym_value_type *p)));
      } catch (mapbox::util::bad_variant_access) {
        *$(text_placements_ptr **ret) = nullptr;
      }
      }|])
    where
      unsafeNewMaybe = mkUnsafeNewMaybe TextPlacements destroyTextPlacements
  pokeV p v' = do
    v <- createTextPlacements v'
    [C.block|void { *$(sym_value_type *p) = sym_value_type(*$fptr-ptr:(text_placements_ptr *v)); }|]


--------------------------------------------------------------------------------
-- * TextSymProperties
--------------------------------------------------------------------------------

foreign import ccall "&hs_mapnik_destroy_TextSymProperties" destroyTextSymProperties :: FinalizerPtr TextSymProperties
foreign import ccall "&hs_mapnik_destroy_Format" destroyFormat :: FinalizerPtr Format

unsafeNewFormat :: MonadBaseControl IO m => (Ptr (Ptr Format) -> m ()) -> m Format
unsafeNewFormat = mkUnsafeNew Format destroyFormat

#define SET_NODE_PROP(TY,HS,CPP) \
    forM_ HS $ \v -> withV v $ \v' -> [C.block|void { \
      auto node = dynamic_cast<TY*>((*$(node_ptr **p))->get()); \
      node->CPP = *$(sym_value_type *v'); \
      }|]

createFormat :: Mapnik.Format -> SvM Format
createFormat f = unsafeNewFormat $ \p -> case f of
  Mapnik.FormatExp (Mapnik.Expression e') ->
    case Expression.parse e' of
      Right e ->
        [C.block|void {
        auto node = std::make_shared<text_node>(*$fptr-ptr:(expression_ptr *e));
        *$(node_ptr **p) = new node_ptr(node);
        }|]
      Left e -> throwIO (userError ("Could not parse format expression" ++ e))
  Mapnik.NullFormat ->
    [C.block|void { *$(node_ptr **p) = new node_ptr(nullptr); }|]
  Mapnik.FormatList fs -> do
    [C.block|void {
    auto node = std::make_shared<list_node>();
    *$(node_ptr **p) = new node_ptr(node);
    }|]
    forM_ fs $ \f' -> do
      f'' <- createFormat f'
      [C.block|void {
        auto parent = static_cast<list_node*>((*$(node_ptr **p))->get());
        parent->push_back(*$fptr-ptr:(node_ptr *f''));
        }|]
  Mapnik.Format {..} -> do
    next' <- createFormat next
    --TODO Handle font
    [C.block|void {
    auto node = std::make_shared<format_node>();
    node->set_child(*$fptr-ptr:(node_ptr *next'));
    *$(node_ptr **p) = new node_ptr(node);
    }|]
    SET_NODE_PROP(format_node, textSize, text_size)
    SET_NODE_PROP(format_node, characterSpacing, character_spacing)
    SET_NODE_PROP(format_node, lineSpacing, line_spacing)
    SET_NODE_PROP(format_node, wrapBefore, wrap_before)
    SET_NODE_PROP(format_node, repeatWrapChar, repeat_wrap_char)
    SET_NODE_PROP(format_node, textTransform, text_transform)
    SET_NODE_PROP(format_node, fill, fill)
    SET_NODE_PROP(format_node, haloFill, halo_fill)
    SET_NODE_PROP(format_node, haloRadius, halo_radius)
    SET_NODE_PROP(format_node, ffSettings, ff_settings)

  Mapnik.FormatLayout {..} -> do
    next' <- createFormat next
    [C.block|void {
    auto node = std::make_shared<layout_node>();
    node->set_child(*$fptr-ptr:(node_ptr *next'));
    *$(node_ptr **p) = new node_ptr(node);
    }|]
    SET_NODE_PROP(layout_node, dx, dx)
    SET_NODE_PROP(layout_node, dy, dy)
    SET_NODE_PROP(layout_node, orientation, orientation)
    SET_NODE_PROP(layout_node, textRatio, text_ratio)
    SET_NODE_PROP(layout_node, wrapWidth, wrap_width)
    SET_NODE_PROP(layout_node, wrapChar, wrap_char)
    SET_NODE_PROP(layout_node, wrapBefore, wrap_before)
    SET_NODE_PROP(layout_node, repeatWrapChar, repeat_wrap_char)
    SET_NODE_PROP(layout_node, rotateDisplacement, rotate_displacement)
    SET_NODE_PROP(layout_node, horizontalAlignment, halign)
    SET_NODE_PROP(layout_node, justifyAlignment, jalign)
    SET_NODE_PROP(layout_node, verticalAlignment, valign)


#define SET_PROP_T(HS,CPP) forM_ HS (\v -> (`pokeV` v) =<< [C.exp|sym_value_type *{ &$(text_properties_expressions *p)->CPP}|])
#define SET_PROP_F(HS,CPP) forM_ HS (\v -> (`pokeV` v) =<< [C.exp|sym_value_type *{ &$(format_properties *p)->CPP}|])
#define SET_PROP_L(HS,CPP) forM_ HS (\v -> (`pokeV` v) =<< [C.exp|sym_value_type *{ &$(text_layout_properties *p)->CPP}|])

withTextProperties
  :: Mapnik.TextProperties -> (Ptr TextProperties -> SvM a) -> SvM a
withTextProperties Mapnik.TextProperties{..} = bracket alloc dealloc . enter
  where
    alloc = [C.exp|text_properties_expressions * { new text_properties_expressions() }|]
    dealloc p = [C.exp|void{delete $(text_properties_expressions *p)}|]
    enter f p = do
      SET_PROP_T(labelPlacement,label_placement)
      SET_PROP_T(labelSpacing,label_spacing)
      SET_PROP_T(labelPositionTolerance,label_position_tolerance)
      SET_PROP_T(avoidEdges,avoid_edges)
      SET_PROP_T(margin,margin)
      SET_PROP_T(repeatDistance,repeat_distance)
      SET_PROP_T(minimumDistance,minimum_distance)
      SET_PROP_T(minimumPathLength,minimum_path_length)
      SET_PROP_T(maxCharAngleDelta,max_char_angle_delta)
      SET_PROP_T(allowOverlap,allow_overlap)
      SET_PROP_T(largestBoxOnly,largest_bbox_only)
      SET_PROP_T(upright,upright)
      f p

withFormatProperties
  :: Mapnik.TextFormatProperties -> (Ptr TextFormatProperties -> SvM a) -> SvM a
withFormatProperties Mapnik.TextFormatProperties{..} = bracket alloc dealloc . enter
  where
    alloc = [C.exp|format_properties * { new format_properties() }|]
    dealloc p = [C.exp|void{delete $(format_properties *p)}|]
    enter f p = do
      forM_ font $ \case
        Mapnik.FaceName (encodeUtf8 -> v) ->
          [C.block|void {
          $(format_properties *p)->face_name = std::string($bs-ptr:v, $bs-len:v);
          }|]
        --TODO fontSet
      SET_PROP_F(textSize,text_size)
      SET_PROP_F(characterSpacing,character_spacing)
      SET_PROP_F(lineSpacing,line_spacing)
      SET_PROP_F(textOpacity,text_opacity)
      SET_PROP_F(haloOpacity,halo_opacity)
      SET_PROP_F(textTransform,text_transform)
      SET_PROP_F(fill,fill)
      SET_PROP_F(haloFill,halo_fill)
      SET_PROP_F(haloRadius,halo_radius)
      SET_PROP_F(ffSettings,ff_settings)
      f p

withLayoutProperties
  :: Mapnik.TextLayoutProperties -> (Ptr TextLayoutProperties -> SvM a) -> SvM a
withLayoutProperties Mapnik.TextLayoutProperties{..} = bracket alloc dealloc . enter
  where
    alloc = [C.exp|text_layout_properties * { new text_layout_properties() }|]
    dealloc p = [C.exp|void{delete $(text_layout_properties *p)}|]
    enter f p = do
      SET_PROP_L(dx,dx)
      SET_PROP_L(dy,dy)
      SET_PROP_L(orientation,orientation)
      SET_PROP_L(textRatio,text_ratio)
      SET_PROP_L(wrapWidth,wrap_width)
      SET_PROP_L(wrapChar,wrap_char)
      SET_PROP_L(wrapBefore,wrap_before)
      SET_PROP_L(repeatWrapChar,repeat_wrap_char)
      SET_PROP_L(rotateDisplacement,rotate_displacement)
      SET_PROP_L(horizontalAlignment,halign)
      SET_PROP_L(justifyAlignment,jalign)
      SET_PROP_L(verticalAlignment,valign)
      forM_ direction $ \(fromIntegral . fromEnum -> v) ->
        [C.block|void { $(text_layout_properties *p)->dir = static_cast<directions_e>($(int v)); }|]
      f p

createTextSymProps :: Mapnik.TextSymProperties -> SvM TextSymProperties
createTextSymProps Mapnik.TextSymProperties{..} = unsafeNewTextSymProps $ \p ->
  createFormat format >>= \fmt ->
  withTextProperties properties $ \props ->
  withFormatProperties formatProperties $ \formatProps ->
  withLayoutProperties layoutProperties $ \layoutProps ->
    [C.block|void {
      text_symbolizer_properties *props =
        *$(text_symbolizer_properties **p) = new text_symbolizer_properties();
      props->expressions     = *$(text_properties_expressions *props);
      props->layout_defaults = *$(text_layout_properties *layoutProps);
      props->format_defaults = *$(format_properties *formatProps);
      props->set_format_tree(*$fptr-ptr:(node_ptr *fmt));
    }|]
  where
    unsafeNewTextSymProps = mkUnsafeNew TextSymProperties destroyTextSymProperties


unCreateTextSymProps :: Ptr TextSymProperties -> SvM Mapnik.TextSymProperties
unCreateTextSymProps ps = do
  format <- unFormat =<< unsafeNewFormat (\p ->
    [C.block|void {
      *$(node_ptr **p) = new node_ptr($(text_symbolizer_properties *ps)->format_tree());
    }|])
  properties <- unProperties =<< [C.exp|text_properties_expressions * {
    &$(text_symbolizer_properties *ps)->expressions
    }|]
  formatProperties <- unFormatProperties =<< [C.exp|format_properties * {
    &$(text_symbolizer_properties *ps)->format_defaults
    }|]
  layoutProperties <- unLayoutProperties =<< [C.exp|text_layout_properties * {
    &$(text_symbolizer_properties *ps)->layout_defaults
    }|]
  return Mapnik.TextSymProperties{..}

#define GET_OPT_PROP(TY,HS,CPP) \
  HS <- readPropWith $ \(has, ret) -> \
    [C.block|void{\
      auto node = dynamic_cast<TY*>($fptr-ptr:(node_ptr *p)->get()); \
      if (node->CPP) {\
        *$(int *has) = 1;\
        *$(sym_value_type** ret) = &(*node->CPP);\
      } else {\
        *$(int *has) = 0;\
      }\
      }|]

unFormat :: Format -> SvM Mapnik.Format
unFormat p = do
  ty <- C.withPtr_ $ \ty ->
    [C.block|void { do {
      *$(int *ty) = -1;
      node_ptr node = *$fptr-ptr:(node_ptr *p);
      if (!node) {
        break;
      }

      text_node *text = dynamic_cast<text_node*>(node.get());
      if (text) {
        *$(int *ty) = 0;
        break;
      }

      format_node *fmt = dynamic_cast<format_node*>(node.get());
      if (fmt) {
        *$(int *ty) = 1;
        break;
      }

      layout_node *lay = dynamic_cast<layout_node*>(node.get());
      if (lay) {
        *$(int *ty) = 2;
        break;
      }

      list_node *lst = dynamic_cast<list_node*>(node.get());
      if (lst) {
        *$(int *ty) = 3;
        break;
      }

    } while(0);}|]
  case ty of
    (-1) -> return Mapnik.NullFormat
    0 -> fmap (Mapnik.FormatExp . Mapnik.Expression) $ newText "unFormat(FormatExp)" $ \(ret,len) ->
        [C.block|void {
          text_node *text = dynamic_cast<text_node*>($fptr-ptr:(node_ptr *p)->get());
          auto exp = text->get_text();
          std::string s = to_expression_string(*exp);
          mallocedString(s, $(char **ret), $(int *len));
        }|]

    1 -> do {
      faceName <- newTextMaybe "unFormat(Format.faceName)" $ \(ret,len) ->
        [C.block|void {
        auto const node = dynamic_cast<format_node*>($fptr-ptr:(node_ptr *p)->get());
        auto v = node->face_name;
        if (v) {
          mallocedString(*v, $(char **ret), $(int *len));
        } else {
          *$(char **ret) = nullptr;
        }
        }|];
      font <- return Nothing; --TODO
      GET_OPT_PROP(format_node, textSize, text_size);
      GET_OPT_PROP(format_node, characterSpacing, character_spacing);
      GET_OPT_PROP(format_node, lineSpacing, line_spacing);
      GET_OPT_PROP(format_node, wrapBefore, wrap_before);
      GET_OPT_PROP(format_node, repeatWrapChar, repeat_wrap_char);
      GET_OPT_PROP(format_node, textTransform, text_transform);
      GET_OPT_PROP(format_node, fill, fill);
      GET_OPT_PROP(format_node, haloFill, halo_fill);
      GET_OPT_PROP(format_node, haloRadius, halo_radius);
      GET_OPT_PROP(format_node, ffSettings, ff_settings);

      next <- unFormat =<< unsafeNewFormat (\pf -> [C.block|void{
        auto const node = dynamic_cast<format_node*>($fptr-ptr:(node_ptr *p)->get());
        *$(node_ptr **pf) =
          new node_ptr(node->get_child());
        }|]);
      return Mapnik.Format{..};
    }

    2 -> do {
      GET_OPT_PROP(layout_node, dx, dx);
      GET_OPT_PROP(layout_node, dy, dy);
      GET_OPT_PROP(layout_node, orientation, orientation);
      GET_OPT_PROP(layout_node, textRatio, text_ratio);
      GET_OPT_PROP(layout_node, wrapWidth, wrap_width);
      GET_OPT_PROP(layout_node, wrapChar, wrap_char);
      GET_OPT_PROP(layout_node, wrapBefore, wrap_before);
      GET_OPT_PROP(layout_node, repeatWrapChar, repeat_wrap_char);
      GET_OPT_PROP(layout_node, rotateDisplacement, rotate_displacement);
      GET_OPT_PROP(layout_node, horizontalAlignment, halign);
      GET_OPT_PROP(layout_node, justifyAlignment, jalign);
      GET_OPT_PROP(layout_node, verticalAlignment, valign);

      next <- unFormat =<< unsafeNewFormat (\pf -> [C.block|void{
        auto const node = dynamic_cast<layout_node*>($fptr-ptr:(node_ptr *p)->get());
        *$(node_ptr **pf) =
          new node_ptr(node->get_child());
        }|]);
      return Mapnik.FormatLayout{..};
    }

    3 -> do
      childRef <- newIORef []
      let callback :: Ptr Format -> IO ()
          callback cp = do
            child <- unsafeNewFormat (`poke` cp)
            modifyIORef' childRef (child:)
      [C.safeBlock|void {
        auto const node = dynamic_cast<list_node*>($fptr-ptr:(node_ptr *p)->get());
        auto children = node->get_children();
        for (auto it=children.begin(); it!=children.end(); ++it) {
          $fun:(void (*callback)(node_ptr *))(new node_ptr(*it));
        }
      }|]
      fmap (Mapnik.FormatList . reverse)
        .  mapM unFormat
        =<< readIORef childRef

    _ -> throwIO (userError "Unsupported node_ptr type")

readPropWith
  :: Variant SymbolizerValue a
  => ((Ptr C.CInt, Ptr (Ptr SymbolizerValue)) -> SvM ()) -> SvM (Maybe a)
readPropWith f = do
  (has,ret) <- C.withPtrs_ f
  if has==1 then Just <$> peekV ret else return Nothing

#define GET_PROP(PS, HS, CPP) \
  HS <- readPropWith $ \(has,ret) -> \
    [C.block|void { \
    const PS def;\
    auto v = *$(sym_value_type **ret) = &$(PS *p)->CPP;\
    *$(int *has) = *v != def.CPP;\
    }|]
#define GET_PROP_F(HS,CPP) GET_PROP(format_properties,HS,CPP)
#define GET_PROP_L(HS,CPP) GET_PROP(text_layout_properties,HS,CPP)
#define GET_PROP_T(HS,CPP) GET_PROP(text_properties_expressions,HS,CPP)

unFormatProperties :: Ptr TextFormatProperties -> SvM Mapnik.TextFormatProperties
unFormatProperties p = do
  faceName <- newTextMaybe "unFormatProperties(faceName)" $ \(ret,len) ->
    [C.block|void {
    const format_properties def;
    auto v = $(format_properties *p)->face_name;
    if (v != def.face_name) {
      mallocedString(v, $(char **ret), $(int *len));
    } else {
      *$(char **ret) = nullptr;
    }
    }|]
  font <- return Nothing --TODO
  GET_PROP_F(textSize, text_size)
  GET_PROP_F(characterSpacing, character_spacing)
  GET_PROP_F(lineSpacing, line_spacing)
  GET_PROP_F(textOpacity, text_opacity)
  GET_PROP_F(haloOpacity, halo_opacity)
  GET_PROP_F(textTransform, text_transform)
  GET_PROP_F(fill, fill)
  GET_PROP_F(haloFill, halo_fill)
  GET_PROP_F(haloRadius, halo_radius)
  GET_PROP_F(ffSettings, ff_settings)
  return Mapnik.TextFormatProperties{..}

unLayoutProperties :: Ptr TextLayoutProperties -> SvM Mapnik.TextLayoutProperties
unLayoutProperties p = do
  GET_PROP_L(dx,dx)
  GET_PROP_L(dy,dy)
  GET_PROP_L(orientation,orientation)
  GET_PROP_L(textRatio,text_ratio)
  GET_PROP_L(wrapWidth,wrap_width)
  GET_PROP_L(wrapChar,wrap_char)
  GET_PROP_L(wrapBefore,wrap_before)
  GET_PROP_L(repeatWrapChar,repeat_wrap_char)
  GET_PROP_L(rotateDisplacement,rotate_displacement)
  GET_PROP_L(horizontalAlignment,halign)
  GET_PROP_L(justifyAlignment,jalign)
  GET_PROP_L(verticalAlignment,valign)
  direction <- fmap (fmap (toEnum . fromIntegral)) $ newMaybe $ \(has,ret) ->
    [C.block|void {
    const text_layout_properties def;
    auto v = $(text_layout_properties *p)->dir;
    if (*$(int *has) = v != def.dir) {
      *$(int *ret) = static_cast<int>(v);
    }
    }|]
  return Mapnik.TextLayoutProperties{..}

unProperties :: Ptr TextProperties -> SvM Mapnik.TextProperties
unProperties p = do
  GET_PROP_T(labelPlacement,label_placement)
  GET_PROP_T(labelSpacing,label_spacing)
  GET_PROP_T(labelPositionTolerance,label_position_tolerance)
  GET_PROP_T(avoidEdges,avoid_edges)
  GET_PROP_T(margin,margin)
  GET_PROP_T(repeatDistance,repeat_distance)
  GET_PROP_T(minimumDistance,minimum_distance)
  GET_PROP_T(minimumPadding,minimum_padding)
  GET_PROP_T(minimumPathLength,minimum_path_length)
  GET_PROP_T(maxCharAngleDelta,max_char_angle_delta)
  GET_PROP_T(allowOverlap,allow_overlap)
  GET_PROP_T(largestBoxOnly,largest_bbox_only)
  GET_PROP_T(upright,upright)
  return Mapnik.TextProperties{..}


--
-- SymbolizerValue Variant instances

instance VariantPtr SymbolizerValue where
  allocaV = bracket alloc dealloc where
    alloc = [C.exp|sym_value_type * { new sym_value_type }|]
    dealloc p = [C.exp|void { delete $(sym_value_type *p)}|]


instance Variant SymbolizerValue Mapnik.Colorizer where
  peekV p = liftBase (unCreateColorizer =<<
    unsafeNewColorizer (\ret ->
      [C.block|void {
      try {
        *$(raster_colorizer_ptr **ret) =
          new raster_colorizer_ptr(util::get<raster_colorizer_ptr>(*$(sym_value_type *p)));
      } catch (mapbox::util::bad_variant_access) {
        *$(raster_colorizer_ptr **ret) = nullptr;
      }
      }|]))
  pokeV p v' = do
    v <- liftBase (createColorizer v');
    [C.block|void {
      *$(sym_value_type *p) = sym_value_type(*$fptr-ptr:(raster_colorizer_ptr *v));
    }|]

instance Variant SymbolizerValue Mapnik.GroupSymProperties where
  peekV p = unCreateGroupSymProperties =<<
    unsafeNewGroupSymProperties (\ret ->
      [C.block|void {
      try {
        *$(group_symbolizer_properties_ptr **ret) =
          new group_symbolizer_properties_ptr(util::get<group_symbolizer_properties_ptr>(*$(sym_value_type *p)));
      } catch (mapbox::util::bad_variant_access) {
        *$(group_symbolizer_properties_ptr **ret) = nullptr;
      }
      }|])
  pokeV p v' = do
    v <- createGroupSymProperties v';
    [C.block|void {
      *$(sym_value_type *p) = sym_value_type(*$fptr-ptr:(group_symbolizer_properties_ptr *v));
    }|]


#define SYM_VAL(HS,CPP,CONV,MSG) \
instance Variant SymbolizerValue HS where {\
  pokeV p (CONV -> v) = \
    [C.block|void { *$(sym_value_type *p) = sym_value_type($(CPP v)); }|]; \
  peekV p = fmap CONV $ justOrTypeError MSG $ newMaybe (\(has,ret) -> \
    [C.block|void { \
    try { \
      *$(CPP *ret) = util::get<CPP>(*$(sym_value_type *p)); \
      *$(int *has) = 1; \
    } catch (mapbox::util::bad_variant_access) { \
      *$(int *has) = 0; \
    } \
    }|]) \
}

#define SYM_VAL_ENUM(HS,CPP) \
instance Variant SymbolizerValue HS where {\
  pokeV p (fromIntegral . fromEnum -> v) = \
    [C.block|void { *$(sym_value_type *p) = enumeration_wrapper(static_cast<CPP>($(int v)));}|]; \
  peekV p = fmap (toEnum . fromIntegral) $ justOrTypeError "sYM_VAL_ENUM" $ newMaybe $ \(has,ret) -> \
    [C.block|void { \
    try { \
      *$(int *ret) = static_cast<int>(util::get<enumeration_wrapper>(*$(sym_value_type *p))); \
      *$(int *has) = 1; \
    } catch (mapbox::util::bad_variant_access) { \
      *$(int *has) = 0; \
    } \
    }|] \
}

SYM_VAL(Int,value_integer,fromIntegral,"Int")
SYM_VAL(Double,double,realToFrac,"Double")
SYM_VAL_ENUM(CompositeMode,composite_mode_e)
SYM_VAL_ENUM(LineCap,line_cap_enum)
SYM_VAL_ENUM(LineJoin,line_join_enum)
SYM_VAL_ENUM(LineRasterizer,line_rasterizer_enum)
SYM_VAL_ENUM(HaloRasterizer,halo_rasterizer_enum)
SYM_VAL_ENUM(PointPlacement,point_placement_enum)
SYM_VAL_ENUM(PatternAlignment,pattern_alignment_enum)
SYM_VAL_ENUM(DebugMode,debug_symbolizer_mode_enum)
SYM_VAL_ENUM(MarkerPlacement,marker_placement_enum)
SYM_VAL_ENUM(MarkerMultiPolicy,marker_multi_policy_enum)
SYM_VAL_ENUM(TextTransform,text_transform_enum)
SYM_VAL_ENUM(LabelPlacement,label_placement_enum)
SYM_VAL_ENUM(VerticalAlignment,vertical_alignment_enum)
SYM_VAL_ENUM(HorizontalAlignment,horizontal_alignment_enum)
SYM_VAL_ENUM(JustifyAlignment,justify_alignment_enum)
SYM_VAL_ENUM(Upright,text_upright_enum)
SYM_VAL_ENUM(Direction,direction_enum)
SYM_VAL_ENUM(GammaMethod,gamma_method_enum)
SYM_VAL_ENUM(ScalingMethod,scaling_method_e)
SYM_VAL_ENUM(SimplifyAlgorithm,simplify_algorithm_e)

instance Variant SymbolizerValue Bool where
  pokeV p (fromIntegral . fromEnum -> v) =
    [C.block|void { *$(sym_value_type *p) = sym_value_type($(int v)?true:false); }|];
  peekV p = fmap (toEnum . fromIntegral) $ justOrTypeError "Bool" $ newMaybe (\(has,ret) ->
    [C.block|void {
    try {
      *$(int *ret) = static_cast<int>(util::get<bool>(*$(sym_value_type *p)));
      *$(int *has) = 1;
    } catch (mapbox::util::bad_variant_access) {
      *$(int *has) = 0;
    }
    }|])

instance Variant SymbolizerValue a => Variant SymbolizerValue (Mapnik.Prop a) where
  peekV p = do
    val <- try (peekV p)
    case val of
      Right v -> return (Mapnik.Exp v)
      Left (VariantTypeError _) -> Mapnik.Val <$> peekV p
  pokeV p (Mapnik.Exp a) = pokeV p a
  pokeV p (Mapnik.Val a) = pokeV p a

instance Variant SymbolizerValue Text where
  peekV p = justOrTypeError "Text" $
    newTextMaybe "peekV(Text)" $ \(ptr, len) ->
      [C.block|void {
      try {
        auto const v = util::get<std::string>(*$(sym_value_type *p));
        mallocedString(v, $(char **ptr), $(int *len));
      } catch (mapbox::util::bad_variant_access) {
        *$(char** ptr) = nullptr;
      }
      }|]
  pokeV p (encodeUtf8 -> v) =
    [C.block|void {
      *$(sym_value_type *p) = sym_value_type(std::string($bs-ptr:v, $bs-len:v));
    }|]

instance Variant SymbolizerValue Char where
  peekV p = do
    s <- peekV p
    case T.uncons s of
      Just (c,_) -> return c
      Nothing    -> throwIO (userError "Unexpected empty string")
  pokeV p (T.singleton -> c) = pokeV p c

instance Variant SymbolizerValue String where
  peekV = fmap unpack . peekV
  pokeV p = pokeV p . pack

instance Variant SymbolizerValue Mapnik.Expression where
  peekV p = 
    fmap Mapnik.Expression $ justOrTypeError "Expression" $ newTextMaybe "peekV(Expression)" $ \(ret, len) ->
      [C.block|void {
      try {
        auto expr = util::get<expression_ptr>(*$(sym_value_type *p));
        if (expr) {
          std::string s = to_expression_string(*expr);
          mallocedString(s, $(char **ret), $(int *len));
        } else {
          *$(char** ret) = nullptr;
        }
      } catch (mapbox::util::bad_variant_access) {
        *$(char** ret) = nullptr;
      }
      }|]
  pokeV p (Mapnik.Expression expr) =
    case Expression.parse expr of
      Right v ->
        [C.block|void{ *$(sym_value_type *p) = sym_value_type(*$fptr-ptr:(expression_ptr *v)); }|]
      Left e -> throwIO (userError e)

instance Variant SymbolizerValue Mapnik.FontFeatureSettings where
  peekV p =
    fmap Mapnik.FontFeatureSettings $ justOrTypeError "FFs" $ newTextMaybe "peekV(FontFeatureSettings)" $ \(ptr, len) ->
      [C.block|void {
      try {
        auto v = util::get<font_feature_settings>(*$(sym_value_type *p));
        std::string s = v.to_string();
        mallocedString(s, $(char **ptr), $(int *len));
      } catch (mapbox::util::bad_variant_access) {
        *$(char** ptr) = nullptr;
      }
      }|]
  pokeV p (Mapnik.FontFeatureSettings (encodeUtf8 -> v)) =
    [C.catchBlock|
      *$(sym_value_type *p) = sym_value_type(font_feature_settings(std::string($bs-ptr:v, $bs-len:v)));
    |]

instance Variant SymbolizerValue Mapnik.DashArray where
  pokeV p dashes =
    [C.block|void {
      std::vector<dash_t> dashes($vec-len:dashes);
      for (int i=0; i<$vec-len:dashes; i++) {
        dashes[i] = $vec-ptr:(dash_t *dashes)[i];
      }
      *$(sym_value_type *p) = sym_value_type(dashes);
    }|]

  peekV p = liftBase $ do
    (fromIntegral -> len, castPtr -> ptr) <- C.withPtrs_ $ \(len,ptr) ->
      [C.block|void{
      try {
        auto arr = util::get<dash_array>(*$(sym_value_type *p));
        *$(size_t *len) = arr.size();
        dash_t *dashes = *$(dash_t **ptr) =
          static_cast<dash_t *>(malloc(arr.size()*sizeof(dash_t)));
        int i=0;
        for (dash_array::const_iterator it=arr.begin(); it!=arr.end(); ++it, ++i) {
          dashes[i] = *it;
        }
      } catch (mapbox::util::bad_variant_access) {
        *$(dash_t **ptr) = nullptr;
      }
      }|]
    if ptr /= nullPtr then do
      fp <- newForeignPtr finalizerFree ptr
      V.freeze (VM.unsafeFromForeignPtr0 fp len)
    else throwIO (VariantTypeError "DashArray")

instance Variant SymbolizerValue Mapnik.Color where
  peekV p =
    justOrTypeError "Color" $ newMaybe $ \(has,ret) ->
      [C.block|void {
      try {
        *$(color *ret) = util::get<color>(*$(sym_value_type *p));
        *$(int *has) = 1;
      } catch (mapbox::util::bad_variant_access) {
        *$(int *has) = 0;
      }
      }|]
  pokeV p c = with c $ \cPtr ->
    [C.block|void {*$(sym_value_type *p) = *$(color *cPtr);}|]

instance Variant SymbolizerValue Mapnik.Transform where
  pokeV p (Mapnik.Transform expr) = liftBase $
    case Transform.parse expr of
      Right v ->
        [C.block|void {*$(sym_value_type *p) = sym_value_type(*$fptr-ptr:(transform_type *v));}|]
      Left e -> throwIO (userError e)

  peekV p = liftBase $
    fmap Mapnik.Transform $ justOrTypeError "Transform" $ newTextMaybe "peekV(Transform)" $ \(ptr, len) ->
      [C.block|void {
      try {
        auto t = util::get<transform_type>(*$(sym_value_type *p));
        if (t) {
          std::string s = transform_processor_type::to_string(*t);
          mallocedString(s, $(char **ptr), $(int *len));
        } else {
          *$(char** ptr) = nullptr;
        }
      } catch (mapbox::util::bad_variant_access) {
        *$(char** ptr) = nullptr;
      }
      }|]

----------------------------------------------------------------------------------------
-- * GroupSymProperties


foreign import ccall "&hs_mapnik_destroy_GroupSymProperties" destroyGroupSymProperties :: FinalizerPtr GroupSymProperties

unsafeNewGroupSymProperties :: MonadBaseControl IO m
  => (Ptr (Ptr GroupSymProperties) -> m ()) -> m GroupSymProperties
unsafeNewGroupSymProperties = mkUnsafeNew GroupSymProperties destroyGroupSymProperties

createGroupSymProperties :: Mapnik.GroupSymProperties -> SvM GroupSymProperties
createGroupSymProperties Mapnik.GroupSymProperties{..} = unsafeNewGroupSymProperties $ \p -> do
  [C.block|void{
    auto ret = std::make_shared<group_symbolizer_properties>();
    *$(group_symbolizer_properties_ptr **p) = new group_symbolizer_properties_ptr(ret);
    }|]
  case layout of
    Mapnik.SimpleRowLayout (realToFrac . fromMaybe defaultSimpleMargin -> margin) ->
      [C.block|void {
      auto sym = (*$(group_symbolizer_properties_ptr **p))->get();
      sym->set_layout(std::move(simple_row_layout($(double margin))));
      }|]
    Mapnik.PairLayout (realToFrac . fromMaybe defaultRowMargin -> margin)
                      (realToFrac . fromMaybe defaultRowMaxDiff -> maxDiff) ->
      [C.block|void {
      auto sym = (*$(group_symbolizer_properties_ptr **p))->get();
      sym->set_layout(std::move(pair_layout($(double margin), $(double maxDiff))));
      }|]
  forM_ rules $ flip withGroupRule $ \r ->
    [C.block|void{
    auto sym = (*$(group_symbolizer_properties_ptr **p))->get();
    sym->add_rule(*$(group_rule_ptr *r));
    }|]

withGroupRule :: Mapnik.GroupRule -> (Ptr Mapnik.GroupRule -> SvM a) -> SvM a
withGroupRule Mapnik.GroupRule{..} f = bracket alloc dealloc enter where
  alloc = [C.exp|group_rule_ptr * { new group_rule_ptr(std::make_shared<group_rule>()) }|]
  dealloc p = [C.exp|void { delete $(group_rule_ptr *p) }|]
  enter p = do
    forM_ filter $ \(Mapnik.Expression e) ->  case Expression.parse e of
      Right e ->
        [C.block|void {
        $(group_rule_ptr *p)->get()->set_filter(*$fptr-ptr:(expression_ptr *e));
        }|]
      Left e -> throwIO (userError ("Could not parse format expression" ++ e))
    forM_ repeatKey $ \(Mapnik.Expression e) ->  case Expression.parse e of
      Right e ->
        [C.block|void {
        $(group_rule_ptr *p)->get()->set_repeat_key(*$fptr-ptr:(expression_ptr *e));
        }|]
      Left e -> throwIO (userError ("Could not parse format expression" ++ e))
    forM_ symbolizers $ create' >=> \sym ->
      [C.block|void {
      $(group_rule_ptr *p)->get()->append(*$fptr-ptr:(symbolizer *sym));
      }|]
    f p

peekGroupRule :: Ptr Mapnik.GroupRule -> SvM Mapnik.GroupRule
peekGroupRule p = do
  filter <- fmap (fmap Mapnik.Expression) $ newTextMaybe "peekGroupRule.filter" $ \(ret,len) ->
    [C.block|void {
    auto r = *$(group_rule_ptr *p);
    auto f = *r->get_filter();
    std::string s = to_expression_string(f);
    static const group_rule dfl;
    std::string sdfl = to_expression_string(*dfl.get_filter());
    if (s != sdfl) {
      mallocedString(s, $(char **ret), $(int *len));
    } else {
      *$(char **ret) = nullptr;
    }
    }|]
  repeatKey <- fmap (fmap Mapnik.Expression) $ newTextMaybe "peekGroupRule.repeatKey" $ \(ret,len) ->
    [C.block|void {
    auto r = *$(group_rule_ptr *p);
    auto f = r->get_repeat_key();
    if (f) {
      std::string s = to_expression_string(*f);
      mallocedString(s, $(char **ret), $(int *len));
    } else {
      *$(char **ret) = nullptr;
    }
    }|]
  symsRef <- newIORef []
  let cb :: Ptr Symbolizer -> IO ()
      cb p = do s <- unsafeNew p
                modifyIORef' symsRef (s:)
  [C.safeBlock|void {
    auto const r = *$(group_rule_ptr *p);
    for (auto it=r->begin(); it!=r->end(); ++it) {
      $fun:(void(*cb)(symbolizer *))(new symbolizer(*it));
    }
  }|]
  symbolizers <- mapM unCreate' =<< (reverse <$> readIORef symsRef)
  return Mapnik.GroupRule {..}

unCreateGroupSymProperties :: GroupSymProperties -> SvM Mapnik.GroupSymProperties
unCreateGroupSymProperties p = do
  layout <- maybe (throwIO (VariantTypeError "GroupLayout")) return
        =<< ((<|>) <$> getRowLayout p <*> getPairLayout p )
  rulesRef <- newIORef []
  env <- ask
  let cb :: Ptr Mapnik.GroupRule -> IO ()
      cb p = do r <- runReaderT (peekGroupRule p) env
                modifyIORef' rulesRef (r:)
  [C.safeBlock|void {
    auto const sym = $fptr-ptr:(group_symbolizer_properties_ptr *p)->get();
    auto rules = sym->get_rules();
    for (auto it=rules.begin(); it!=rules.end(); ++it) {
      $fun:(void (*cb)(group_rule_ptr *))(&*it);
    }
  }|]
  rules <- reverse <$> readIORef rulesRef
  return Mapnik.GroupSymProperties{..}
  where
    getPairLayout p = do
      (is, realToFrac -> margin, realToFrac -> maxDiff) <- C.withPtrs_ $ \(is,margin,maxDiff) ->
        [C.block|void {
          try {
            auto const sym = $fptr-ptr:(group_symbolizer_properties_ptr *p)->get();
            auto v = util::get<pair_layout>(sym->get_layout());
            *$(int *is) = 1;
            *$(double *margin) = v.get_item_margin();
            *$(double *maxDiff) = v.get_max_difference();
          } catch (mapbox::util::bad_variant_access) {
            *$(int *is) = 0;
          }
        }|]
      let itemMargin = if margin==defaultRowMargin then Nothing else Just margin
          maxDifference = if maxDiff==defaultRowMaxDiff then Nothing else Just maxDiff
      return $! if is==1 then Just Mapnik.PairLayout{..} else Nothing

    getRowLayout p = do
      (is, realToFrac -> margin) <- C.withPtrs_ $ \(is,margin) ->
        [C.block|void {
          try {
            auto const sym = $fptr-ptr:(group_symbolizer_properties_ptr *p)->get();
            auto v = util::get<simple_row_layout>(sym->get_layout());
            *$(int *is) = 1;
            *$(double *margin) = v.get_item_margin();
          } catch (mapbox::util::bad_variant_access) {
            *$(int *is) = 0;
          }
        }|]
      let itemMargin = if margin==defaultSimpleMargin then Nothing else Just margin
      return $! if is==1 then Just Mapnik.SimpleRowLayout{..} else Nothing
      
defaultSimpleMargin :: Double
defaultSimpleMargin = 0

defaultRowMargin :: Double
defaultRowMargin = 1

defaultRowMaxDiff :: Double
defaultRowMaxDiff = -1
