
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Mapnik.Bindings.Symbolizer (
  Symbolizer
, create
, unCreate
, unsafeNew
) where

import qualified Mapnik
import           Mapnik (Property, DSum(..), Key(..), toProperties, PropValue(..))
import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import qualified Mapnik.Bindings.Color as Color
import qualified Mapnik.Bindings.Expression as Expression

import           Control.Exception
import           Data.Maybe (catMaybes)
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Data.String (fromString)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable)
import           GHC.Exts (fromList)

import qualified Language.C.Inline.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/symbolizer.hpp>"
C.include "<mapnik/symbolizer_utils.hpp>"
C.include "<mapnik/symbolizer_keys.hpp>"
C.include "<mapnik/expression_string.hpp>"
C.include "<mapnik/expression_evaluator.hpp>"
C.include "symbolizer_util.hpp"

C.using "namespace mapnik"

--
-- * Symbolizer


foreign import ccall "&hs_mapnik_destroy_Symbolizer" destroySymbolizer :: FinalizerPtr Symbolizer

unsafeNew :: Ptr Symbolizer -> IO Symbolizer
unsafeNew = fmap Symbolizer . newForeignPtr destroySymbolizer

create :: Mapnik.Symbolizer -> IO Symbolizer
create sym = bracket alloc dealloc $ \p -> do
  mapM_ (flip setProperty p) (toProperties sym)
  unsafeNew =<< castSym sym p
  where
    alloc = [C.exp|symbolizer_base * { new symbolizer_base() }|]
    dealloc p = [C.block|void { delete $(symbolizer_base *p);}|]

unCreate :: Symbolizer -> IO Mapnik.Symbolizer
unCreate sym' = bracket alloc dealloc $ \sym -> do
  props <- fmap (fromList . catMaybes) $ sequence
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
    , getProperty TextPlacements_ sym
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
  name <- getName sym'
  case name of
    "PointSymbolizer" -> return (Mapnik.Point props)
    "LineSymbolizer" -> return (Mapnik.Line props)
    "LinePatternSymbolizer" -> return (Mapnik.LinePattern props)
    "PolygonSymbolizer" -> return (Mapnik.Polygon props)
    "PolygonPatternSymbolizer" -> return (Mapnik.PolygonPattern props)
    "RasterSymbolizer" -> return (Mapnik.Raster props)
    "ShieldSymbolizer" -> return (Mapnik.Shield props)
    "TextSymbolizer" -> return (Mapnik.Text props)
    "BuildingSymbolizer" -> return (Mapnik.Building props)
    "MarkersSymbolizer" -> return (Mapnik.Marker props)
    "GroupSymbolizer" -> return (Mapnik.Group props)
    "DebugSymbolizer" -> return (Mapnik.Debug props)
    "DotSymbolizer" -> return (Mapnik.Dot props)
    _ -> throwIO (userError "Unexpected symbolizer name")
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
castSym Mapnik.Marker{} p =
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

getMaybe :: Storable a => ((Ptr C.CInt, Ptr a) -> IO ()) -> IO (Maybe a)
getMaybe fun = do
  (has, p) <- C.withPtrs_ fun
  return $ if has == 1 then Just p else Nothing

instance HasGetProp Double where
  getProp (keyIndex -> k) sym = fmap (fmap realToFrac) <$> getMaybe $ \(has,p) ->
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
  setProp (keyIndex -> k) ((realToFrac -> v)) s = 
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = $(double v);}|]

instance HasSetProp () where setProp _ _ _ = return ()
instance HasGetProp () where getProp _ _ = return Nothing

instance HasGetProp Mapnik.Color where
  getProp (keyIndex -> k) sym = Color.mkCreateMaybe $ \(has,r,g,b,a) ->
    [C.block|void {
    auto val = get_optional<color>(*$(symbolizer_base *sym), $(keys k));
    if (val) {
      *$(int *has) = 1;
      *$(unsigned char *r) = val->red();
      *$(unsigned char *g) = val->green();
      *$(unsigned char *b) = val->blue();
      *$(unsigned char *a) = val->alpha();
    } else {
      *$(int *has) = 0;
    }
    }|]

instance HasSetProp Mapnik.Color where
  setProp (keyIndex -> k) (color) s = do
    c <- createColor color
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = *$fptr-ptr:(color *c);}|]

instance HasGetProp Bool where
  getProp (keyIndex -> k) sym = fmap (fmap (toEnum . fromIntegral)) <$> getMaybe $ \(has,p) ->
    [C.block|void {
    auto val = get_optional<bool>(*$(symbolizer_base *sym), $(keys k));
    if (val) {
      *$(int *has) = 1;
      *$(int *p) = *val;
    } else {
      *$(int *has) = 0;
    }
    }|]

instance HasSetProp Bool where
  setProp (keyIndex -> k) ((fromIntegral . fromEnum -> v)) s =
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = $(int v)?true:false;}|]

instance HasGetProp Int where
  getProp (keyIndex -> k) sym = fmap (fmap fromIntegral) <$> getMaybe $ \(has,p) ->
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
  setProp (keyIndex -> k) ((fromIntegral -> v)) s =
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
  setProp (keyIndex -> k) ((encodeUtf8 -> v)) s =
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
  setProp (keyIndex -> k) ((fromString -> v)) s =
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = std::string($bs-ptr:v, $bs-len:v);}|]


instance HasGetProp Mapnik.Expression where getProp = getPropExpression
instance HasSetProp Mapnik.Expression where setProp = setPropExpression


setProp' :: HasSetProp a
         => Key a -> PropValue a -> Ptr SymbolizerBase -> IO ()
setProp' k (PropValue v) = setProp k v
setProp' k (PropExpression v) = setPropExpression k v

getProp' :: HasGetProp a
         => Key a -> Ptr SymbolizerBase -> IO (Maybe (PropValue a))
getProp' k p = do
  mVal <- getProp k p
  case mVal of
    Nothing -> fmap PropExpression <$> getPropExpression k p
    Just a -> return (Just (PropValue a))

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
    if (expr) {
      std::string s = to_expression_string(**expr);
      *$(char** ptr) = strdup(s.c_str());
      *$(int* len) = s.length();
    } else {
      *$(char** ptr) = NULL;
    }
    }|]

getProperty :: Key a -> Ptr SymbolizerBase -> IO (Maybe Property)
getProperty Gamma = fmap (fmap (Gamma :=>)) . getProp' Gamma
getProperty GammaMethod = fmap (fmap (GammaMethod :=>)) . getProp' GammaMethod
getProperty Opacity = fmap (fmap (Opacity :=>)) . getProp' Opacity
getProperty Alignment = fmap (fmap (Alignment :=>)) . getProp' Alignment
getProperty Offset = fmap (fmap (Offset :=>)) . getProp' Offset
getProperty CompOp = fmap (fmap (CompOp :=>)) . getProp' CompOp
getProperty Clip = fmap (fmap (Clip :=>)) . getProp' Clip
getProperty Fill = fmap (fmap (Fill :=>)) . getProp' Fill
getProperty FillOpacity = fmap (fmap (FillOpacity :=>)) . getProp' FillOpacity
getProperty Stroke = fmap (fmap (Stroke :=>)) . getProp' Stroke
getProperty StrokeWidth = fmap (fmap (StrokeWidth :=>)) . getProp' StrokeWidth
getProperty StrokeOpacity = fmap (fmap (StrokeOpacity :=>)) . getProp' StrokeOpacity
getProperty StrokeLinejoin = fmap (fmap (StrokeLinejoin :=>)) . getProp' StrokeLinejoin
getProperty StrokeLinecap = fmap (fmap (StrokeLinecap :=>)) . getProp' StrokeLinecap
getProperty StrokeGamma = fmap (fmap (StrokeGamma :=>)) . getProp' StrokeGamma
getProperty StrokeGammaMethod = fmap (fmap (StrokeGammaMethod :=>)) . getProp' StrokeGammaMethod
getProperty StrokeDashoffset = fmap (fmap (StrokeDashoffset :=>)) . getProp' StrokeDashoffset
getProperty StrokeDasharray = fmap (fmap (StrokeDasharray :=>)) . getProp' StrokeDasharray
getProperty StrokeMiterlimit = fmap (fmap (StrokeMiterlimit :=>)) . getProp' StrokeMiterlimit
getProperty GeometryTransform = fmap (fmap (GeometryTransform :=>)) . getProp' GeometryTransform
getProperty LineRasterizer = fmap (fmap (LineRasterizer :=>)) . getProp' LineRasterizer
getProperty ImageTransform = fmap (fmap (ImageTransform :=>)) . getProp' ImageTransform
getProperty Spacing = fmap (fmap (Spacing :=>)) . getProp' Spacing
getProperty MaxError = fmap (fmap (MaxError :=>)) . getProp' MaxError
getProperty AllowOverlap = fmap (fmap (AllowOverlap :=>)) . getProp' AllowOverlap
getProperty IgnorePlacement = fmap (fmap (IgnorePlacement :=>)) . getProp' IgnorePlacement
getProperty Width = fmap (fmap (Width :=>)) . getProp' Width
getProperty Height = fmap (fmap (Height :=>)) . getProp' Height
getProperty File = fmap (fmap (File :=>)) . getProp' File
getProperty ShieldDx = fmap (fmap (ShieldDx :=>)) . getProp' ShieldDx
getProperty ShieldDy = fmap (fmap (ShieldDy :=>)) . getProp' ShieldDy
getProperty UnlockImage = fmap (fmap (UnlockImage :=>)) . getProp' UnlockImage
getProperty Mode = fmap (fmap (Mode :=>)) . getProp' Mode
getProperty Scaling = fmap (fmap (Scaling :=>)) . getProp' Scaling
getProperty FilterFactor = fmap (fmap (FilterFactor :=>)) . getProp' FilterFactor
getProperty MeshSize = fmap (fmap (MeshSize :=>)) . getProp' MeshSize
getProperty Premultiplied = fmap (fmap (Premultiplied :=>)) . getProp' Premultiplied
getProperty Smooth = fmap (fmap (Smooth :=>)) . getProp' Smooth
getProperty SimplifyAlgorithm = fmap (fmap (SimplifyAlgorithm :=>)) . getProp' SimplifyAlgorithm
getProperty SimplifyTolerance = fmap (fmap (SimplifyTolerance :=>)) . getProp' SimplifyTolerance
getProperty HaloRasterizer = fmap (fmap (HaloRasterizer :=>)) . getProp' HaloRasterizer
getProperty TextPlacements_ = fmap (fmap (TextPlacements_ :=>)) . getProp' TextPlacements_
getProperty LabelPlacement = fmap (fmap (LabelPlacement :=>)) . getProp' LabelPlacement
getProperty MarkersPlacementType = fmap (fmap (MarkersPlacementType :=>)) . getProp' MarkersPlacementType
getProperty MarkersMultipolicy = fmap (fmap (MarkersMultipolicy :=>)) . getProp' MarkersMultipolicy
getProperty PointPlacementType = fmap (fmap (PointPlacementType :=>)) . getProp' PointPlacementType
getProperty Colorizer = fmap (fmap (Colorizer :=>)) . getProp' Colorizer
getProperty HaloTransform = fmap (fmap (HaloTransform :=>)) . getProp' HaloTransform
getProperty NumColumns = fmap (fmap (NumColumns :=>)) . getProp' NumColumns
getProperty StartColumn = fmap (fmap (StartColumn :=>)) . getProp' StartColumn
getProperty RepeatKey = fmap (fmap (RepeatKey :=>)) . getProp' RepeatKey
getProperty GroupProperties = fmap (fmap (GroupProperties :=>)) . getProp' GroupProperties
getProperty LargestBoxOnly = fmap (fmap (LargestBoxOnly :=>)) . getProp' LargestBoxOnly
getProperty MinimumPathLength = fmap (fmap (MinimumPathLength :=>)) . getProp' MinimumPathLength
getProperty HaloCompOp = fmap (fmap (HaloCompOp :=>)) . getProp' HaloCompOp
getProperty TextTransform = fmap (fmap (TextTransform :=>)) . getProp' TextTransform
getProperty HorizontalAlignment = fmap (fmap (HorizontalAlignment :=>)) . getProp' HorizontalAlignment
getProperty JustifyAlignment = fmap (fmap (JustifyAlignment :=>)) . getProp' JustifyAlignment
getProperty VerticalAlignment = fmap (fmap (VerticalAlignment :=>)) . getProp' VerticalAlignment
getProperty Upright = fmap (fmap (Upright :=>)) . getProp' Upright
getProperty Direction = fmap (fmap (Direction :=>)) . getProp' Direction
getProperty AvoidEdges = fmap (fmap (AvoidEdges :=>)) . getProp' AvoidEdges
getProperty FfSettings = fmap (fmap (FfSettings :=>)) . getProp' FfSettings

setProperty :: Property -> Ptr SymbolizerBase -> IO ()
setProperty (Gamma :=> v) = setProp' Gamma v
setProperty (GammaMethod :=> v) = setProp' GammaMethod v
setProperty (Opacity :=> v) = setProp' Opacity v
setProperty (Alignment :=> v) = setProp' Alignment v
setProperty (Offset :=> v) = setProp' Offset v
setProperty (CompOp :=> v) = setProp' CompOp v
setProperty (Clip :=> v) = setProp' Clip v
setProperty (Fill :=> v) = setProp' Fill v
setProperty (FillOpacity :=> v) = setProp' FillOpacity v
setProperty (Stroke :=> v) = setProp' Stroke v
setProperty (StrokeWidth :=> v) = setProp' StrokeWidth v
setProperty (StrokeOpacity :=> v) = setProp' StrokeOpacity v
setProperty (StrokeLinejoin :=> v) = setProp' StrokeLinejoin v
setProperty (StrokeLinecap :=> v) = setProp' StrokeLinecap v
setProperty (StrokeGamma :=> v) = setProp' StrokeGamma v
setProperty (StrokeGammaMethod :=> v) = setProp' StrokeGammaMethod v
setProperty (StrokeDashoffset :=> v) = setProp' StrokeDashoffset v
setProperty (StrokeDasharray :=> v) = setProp' StrokeDasharray v
setProperty (StrokeMiterlimit :=> v) = setProp' StrokeMiterlimit v
setProperty (GeometryTransform :=> v) = setProp' GeometryTransform v
setProperty (LineRasterizer :=> v) = setProp' LineRasterizer v
setProperty (ImageTransform :=> v) = setProp' ImageTransform v
setProperty (Spacing :=> v) = setProp' Spacing v
setProperty (MaxError :=> v) = setProp' MaxError v
setProperty (AllowOverlap :=> v) = setProp' AllowOverlap v
setProperty (IgnorePlacement :=> v) = setProp' IgnorePlacement v
setProperty (Width :=> v) = setProp' Width v
setProperty (Height :=> v) = setProp' Height v
setProperty (File :=> v) = setProp' File v
setProperty (ShieldDx :=> v) = setProp' ShieldDx v
setProperty (ShieldDy :=> v) = setProp' ShieldDy v
setProperty (UnlockImage :=> v) = setProp' UnlockImage v
setProperty (Mode :=> v) = setProp' Mode v
setProperty (Scaling :=> v) = setProp' Scaling v
setProperty (FilterFactor :=> v) = setProp' FilterFactor v
setProperty (MeshSize :=> v) = setProp' MeshSize v
setProperty (Premultiplied :=> v) = setProp' Premultiplied v
setProperty (Smooth :=> v) = setProp' Smooth v
setProperty (SimplifyAlgorithm :=> v) = setProp' SimplifyAlgorithm v
setProperty (SimplifyTolerance :=> v) = setProp' SimplifyTolerance v
setProperty (HaloRasterizer :=> v) = setProp' HaloRasterizer v
setProperty (TextPlacements_ :=> v) = setProp' TextPlacements_ v
setProperty (LabelPlacement :=> v) = setProp' LabelPlacement v
setProperty (MarkersPlacementType :=> v) = setProp' MarkersPlacementType v
setProperty (MarkersMultipolicy :=> v) = setProp' MarkersMultipolicy v
setProperty (PointPlacementType :=> v) = setProp' PointPlacementType v
setProperty (Colorizer :=> v) = setProp' Colorizer v
setProperty (HaloTransform :=> v) = setProp' HaloTransform v
setProperty (NumColumns :=> v) = setProp' NumColumns v
setProperty (StartColumn :=> v) = setProp' StartColumn v
setProperty (RepeatKey :=> v) = setProp' RepeatKey v
setProperty (GroupProperties :=> v) = setProp' GroupProperties v
setProperty (LargestBoxOnly :=> v) = setProp' LargestBoxOnly v
setProperty (MinimumPathLength :=> v) = setProp' MinimumPathLength v
setProperty (HaloCompOp :=> v) = setProp' HaloCompOp v
setProperty (TextTransform :=> v) = setProp' TextTransform v
setProperty (HorizontalAlignment :=> v) = setProp' HorizontalAlignment v
setProperty (JustifyAlignment :=> v) = setProp' JustifyAlignment v
setProperty (VerticalAlignment :=> v) = setProp' VerticalAlignment v
setProperty (Upright :=> v) = setProp' Upright v
setProperty (Direction :=> v) = setProp' Direction v
setProperty (AvoidEdges :=> v) = setProp' AvoidEdges v
setProperty (FfSettings :=> v) = setProp' FfSettings v

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
keyIndex TextPlacements_ = [C.pure|keys{keys::text_placements_}|]
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

createColor :: Mapnik.Color -> IO Color
createColor = maybe (throwIO (userError "setProperty: Invalid Color")) return . Color.create

