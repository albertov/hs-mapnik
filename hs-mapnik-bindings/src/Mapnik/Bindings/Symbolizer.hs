
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Mapnik.Bindings.Symbolizer (
  Symbolizer
, create
) where

import qualified Mapnik
import           Mapnik (Property(..), Key(..), _symbolizerProperties, PropValue(..))
import           Mapnik.Bindings
import qualified Mapnik.Bindings.Color as Color
import qualified Mapnik.Bindings.Expression as Expression

import           Control.Exception
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.String (fromString)
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/symbolizer.hpp>"
C.include "<mapnik/symbolizer_keys.hpp>"

C.using "namespace mapnik"

--
-- * Symbolizer


foreign import ccall "&hs_mapnik_destroy_Symbolizer" destroySymbolizer :: FinalizerPtr Symbolizer

newSym :: Ptr Symbolizer -> IO Symbolizer
newSym = fmap Symbolizer . newForeignPtr destroySymbolizer

create :: Mapnik.Symbolizer -> IO Symbolizer
create sym = bracket alloc dealloc $ \p -> do
  mapM_ (flip setProperty p) (_symbolizerProperties sym)
  newSym =<< castSym sym p
  where
    alloc = [C.exp|symbolizer_base * { new symbolizer_base() }|]
    dealloc p = [C.block|void { delete $(symbolizer_base *p);}|]

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
  setProp :: Key a -> PropValue a -> Ptr SymbolizerBase -> IO ()

instance HasSetProp Double where
  setProp (keyIndex -> k) (PropValue (realToFrac -> v)) s = 
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = $(double v);}|]
  setProp k (PropExpression v) s = setPropExpression k v s

instance HasSetProp () where setProp _ _ _ = return ()

instance HasSetProp Mapnik.Color where
  setProp (keyIndex -> k) (PropValue color) s = do
    c <- createColor color
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = *$fptr-ptr:(color *c);}|]
  setProp k (PropExpression v) s = setPropExpression k v s

instance HasSetProp Bool where
  setProp (keyIndex -> k) (PropValue (fromIntegral . fromEnum -> v)) s =
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = $(int v)?true:false;}|]
  setProp k (PropExpression v) s = setPropExpression k v s

instance HasSetProp Int where
  setProp (keyIndex -> k) (PropValue (fromIntegral -> v)) s =
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = $(int v);}|]
  setProp k (PropExpression v) s = setPropExpression k v s

instance HasSetProp Text where
  setProp (keyIndex -> k) (PropValue (encodeUtf8 -> v)) s =
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = std::string($bs-ptr:v, $bs-len:v);}|]
  setProp k (PropExpression v) s = setPropExpression k v s

instance HasSetProp String where
  setProp (keyIndex -> k) (PropValue (fromString -> v)) s =
    [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = std::string($bs-ptr:v, $bs-len:v);}|]
  setProp k (PropExpression v) s = setPropExpression k v s


setPropExpression :: Key a
                  -> Mapnik.Expression -> Ptr SymbolizerBase -> IO ()
setPropExpression (keyIndex -> k) (Mapnik.Expression expr) s =
  case Expression.parse expr of
    Right v ->
      [C.block|void {$(symbolizer_base *s)->properties[$(keys k)] = *$fptr-ptr:(expression_ptr *v);}|]
    Left e -> throwIO (userError e)

setProperty :: Property -> Ptr SymbolizerBase -> IO ()
setProperty (Gamma :=> v) = setProp Gamma v
setProperty (GammaMethod :=> v) = setProp GammaMethod v
setProperty (Opacity :=> v) = setProp Opacity v
setProperty (Alignment :=> v) = setProp Alignment v
setProperty (Offset :=> v) = setProp Offset v
setProperty (CompOp :=> v) = setProp CompOp v
setProperty (Clip :=> v) = setProp Clip v
setProperty (Fill :=> v) = setProp Fill v
setProperty (FillOpacity :=> v) = setProp FillOpacity v
setProperty (Stroke :=> v) = setProp Stroke v
setProperty (StrokeWidth :=> v) = setProp StrokeWidth v
setProperty (StrokeOpacity :=> v) = setProp StrokeOpacity v
setProperty (StrokeLinejoin :=> v) = setProp StrokeLinejoin v
setProperty (StrokeLinecap :=> v) = setProp StrokeLinecap v
setProperty (StrokeGamma :=> v) = setProp StrokeGamma v
setProperty (StrokeGammaMethod :=> v) = setProp StrokeGammaMethod v
setProperty (StrokeDashoffset :=> v) = setProp StrokeDashoffset v
setProperty (StrokeDasharray :=> v) = setProp StrokeDasharray v
setProperty (StrokeMiterlimit :=> v) = setProp StrokeMiterlimit v
setProperty (GeometryTransform :=> v) = setProp GeometryTransform v
setProperty (LineRasterizer :=> v) = setProp LineRasterizer v
setProperty (ImageTransform :=> v) = setProp ImageTransform v
setProperty (Spacing :=> v) = setProp Spacing v
setProperty (MaxError :=> v) = setProp MaxError v
setProperty (AllowOverlap :=> v) = setProp AllowOverlap v
setProperty (IgnorePlacement :=> v) = setProp IgnorePlacement v
setProperty (Width :=> v) = setProp Width v
setProperty (Height :=> v) = setProp Height v
setProperty (File :=> v) = setProp File v
setProperty (ShieldDx :=> v) = setProp ShieldDx v
setProperty (ShieldDy :=> v) = setProp ShieldDy v
setProperty (UnlockImage :=> v) = setProp UnlockImage v
setProperty (Mode :=> v) = setProp Mode v
setProperty (Scaling :=> v) = setProp Scaling v
setProperty (FilterFactor :=> v) = setProp FilterFactor v
setProperty (MeshSize :=> v) = setProp MeshSize v
setProperty (Premultiplied :=> v) = setProp Premultiplied v
setProperty (Smooth :=> v) = setProp Smooth v
setProperty (SimplifyAlgorithm :=> v) = setProp SimplifyAlgorithm v
setProperty (SimplifyTolerance :=> v) = setProp SimplifyTolerance v
setProperty (HaloRasterizer :=> v) = setProp HaloRasterizer v
setProperty (TextPlacements_ :=> v) = setProp TextPlacements_ v
setProperty (LabelPlacement :=> v) = setProp LabelPlacement v
setProperty (MarkersPlacementType :=> v) = setProp MarkersPlacementType v
setProperty (MarkersMultipolicy :=> v) = setProp MarkersMultipolicy v
setProperty (PointPlacementType :=> v) = setProp PointPlacementType v
setProperty (Colorizer :=> v) = setProp Colorizer v
setProperty (HaloTransform :=> v) = setProp HaloTransform v
setProperty (NumColumns :=> v) = setProp NumColumns v
setProperty (StartColumn :=> v) = setProp StartColumn v
setProperty (RepeatKey :=> v) = setProp RepeatKey v
setProperty (GroupProperties :=> v) = setProp GroupProperties v
setProperty (LargestBoxOnly :=> v) = setProp LargestBoxOnly v
setProperty (MinimumPathLength :=> v) = setProp MinimumPathLength v
setProperty (HaloCompOp :=> v) = setProp HaloCompOp v
setProperty (TextTransform :=> v) = setProp TextTransform v
setProperty (HorizontalAlignment :=> v) = setProp HorizontalAlignment v
setProperty (JustifyAlignment :=> v) = setProp JustifyAlignment v
setProperty (VerticalAlignment :=> v) = setProp VerticalAlignment v
setProperty (Upright :=> v) = setProp Upright v
setProperty (Direction :=> v) = setProp Direction v
setProperty (AvoidEdges :=> v) = setProp AvoidEdges v
setProperty (FfSettings :=> v) = setProp FfSettings v

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

