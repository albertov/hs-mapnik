{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Mapnik.Bindings.TextSymProperties (
  unsafeNew
, unsafeNewMaybe
, create
, unCreate
) where

import qualified Mapnik
import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.SymbolizerValue (SymValue(..))
import qualified Mapnik.Bindings.Expression as Expression

import           Control.Exception (bracket, throwIO)
import           Control.Monad (forM_)
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr, withForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/text/text_properties.hpp>"
C.include "<mapnik/text/formatting/base.hpp>"
C.include "<mapnik/text/formatting/text.hpp>"

C.using "namespace mapnik"
C.using "namespace mapnik::formatting"
C.verbatim "typedef symbolizer_base::value_type sym_value_type;"

-- * TextSymProperties


foreign import ccall "&hs_mapnik_destroy_TextSymProperties" destroyTextSymProperties :: FinalizerPtr TextSymProperties
foreign import ccall "&hs_mapnik_destroy_Format" destroyFormat :: FinalizerPtr Format

unsafeNew :: (Ptr (Ptr TextSymProperties) -> IO ()) -> IO TextSymProperties
unsafeNew = mkUnsafeNew TextSymProperties destroyTextSymProperties

unsafeNewMaybe :: (Ptr (Ptr TextSymProperties) -> IO ()) -> IO (Maybe TextSymProperties)
unsafeNewMaybe = mkUnsafeNewMaybe TextSymProperties destroyTextSymProperties

parseExp :: Mapnik.Expression -> Either String Expression
parseExp (Mapnik.Expression e) = Expression.parse e

withFormat :: Mapnik.Format -> (Ptr Format -> IO a) -> IO a
withFormat (Mapnik.FormatExp (parseExp -> Right e)) = bracket alloc dealloc
  where
    alloc = [C.block|node_ptr * {
      auto node = std::make_shared<text_node>(*$fptr-ptr:(expression_ptr *e));
      new node_ptr(node);
      }|]
    dealloc p = [C.exp|void{delete $(node_ptr *p)}|]
withFormat (Mapnik.FormatExp _) =
  const (throwIO (userError "Could not parse format expression"))
withFormat Mapnik.NullFormat = bracket alloc dealloc
  where 
    alloc = [C.exp|node_ptr * { nullptr }|]
    dealloc = const (return ())


#define SET_PROP_T(HS,CPP) forM_ HS (\v -> (`pokeSv` v) =<< [C.exp|sym_value_type *{ &$(text_properties_expressions *p)->CPP}|])
#define SET_PROP_F(HS,CPP) forM_ HS (\v -> (`pokeSv` v) =<< [C.exp|sym_value_type *{ &$(format_properties *p)->CPP}|])
#define SET_PROP_L(HS,CPP) forM_ HS (\v -> (`pokeSv` v) =<< [C.exp|sym_value_type *{ &$(text_layout_properties *p)->CPP}|])

withTextProperties :: Mapnik.TextProperties -> (Ptr TextProperties -> IO a) -> IO a
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

withFormatProperties :: Mapnik.TextFormatProperties -> (Ptr TextFormatProperties -> IO a) -> IO a
withFormatProperties Mapnik.TextFormatProperties{..} = bracket alloc dealloc . enter
  where
    alloc = [C.exp|format_properties * { new format_properties() }|]
    dealloc p = [C.exp|void{delete $(format_properties *p)}|]
    enter f p = do
      forM_ faceName $ \(encodeUtf8 -> v) ->
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

withLayoutProperties :: Mapnik.TextLayoutProperties -> (Ptr TextLayoutProperties -> IO a) -> IO a
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

create :: Mapnik.TextSymProperties -> IO TextSymProperties
create Mapnik.TextSymProperties{..} = unsafeNew $ \p ->
  withFormat format $ \fmt ->
  withTextProperties properties $ \props ->
  withFormatProperties formatProperties $ \formatProps ->
  withLayoutProperties layoutProperties $ \layoutProps ->
    [C.block|void {
      text_symbolizer_properties *props =
        *$(text_symbolizer_properties **p) = new text_symbolizer_properties();
      props->expressions     = *$(text_properties_expressions *props);
      props->layout_defaults = *$(text_layout_properties *layoutProps);
      props->format_defaults = *$(format_properties *formatProps);
      if ($(node_ptr *fmt)) {
        props->set_format_tree(*$(node_ptr *fmt));
      }
    }|]

unCreate :: TextSymProperties -> IO Mapnik.TextSymProperties
unCreate (TextSymProperties props) = withForeignPtr props $ \ps -> do
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

unFormat :: Format -> IO Mapnik.Format
unFormat = undefined

readPropWith
  :: SymValue a
  => ((Ptr C.CInt, Ptr (Ptr SymbolizerValue)) -> IO ()) -> IO (Maybe a)
readPropWith f = do
  (has,ret) <- C.withPtrs_ f
  if has==1 then peekSv ret else return Nothing

#define GET_PROP(PS, HS, CPP) \
  HS <- readPropWith $ \(has,ret) -> \
    [C.block|void { \
    static const PS def;\
    auto v = *$(sym_value_type **ret) = &$(PS *p)->CPP;\
    *$(int *has) = *v != def.CPP;\
    }|]
#define GET_PROP_F(HS,CPP) GET_PROP(format_properties,HS,CPP)

unFormatProperties :: Ptr TextFormatProperties -> IO Mapnik.TextFormatProperties
unFormatProperties p = do
  faceName <- newTextMaybe $ \(ret,len) ->
    [C.block|void {
    static const format_properties def;
    auto v = $(format_properties *p)->face_name;
    if (v != def.face_name) {
      *$(char **ret) = strdup(v.c_str());
      *$(int *len) = v.size();
    } else {
      *$(char **ret) = nullptr;
    }
    }|]
  fontSet <- return Nothing --TODO
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

#define GET_PROP_L(HS,CPP) GET_PROP(text_layout_properties,HS,CPP)
unLayoutProperties :: Ptr TextLayoutProperties -> IO Mapnik.TextLayoutProperties
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
    static const text_layout_properties def;
    auto v = $(text_layout_properties *p)->dir;
    if (*$(int *has) = v != def.dir) {
      *$(int *ret) = static_cast<int>(v);
    }
    }|]
  return Mapnik.TextLayoutProperties{..}

#define GET_PROP_T(HS,CPP) GET_PROP(text_properties_expressions,HS,CPP)
unProperties :: Ptr TextProperties -> IO Mapnik.TextProperties
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

unsafeNewFormat :: (Ptr (Ptr Format) -> IO ()) -> IO Format
unsafeNewFormat = mkUnsafeNew Format destroyFormat
