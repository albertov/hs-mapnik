{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import qualified Mapnik.Bindings.Expression as Expression

import           Control.Exception (bracket, throwIO)
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C

C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/text/text_properties.hpp>"
C.include "<mapnik/text/formatting/base.hpp>"
C.include "<mapnik/text/formatting/text.hpp>"

C.using "namespace mapnik"
C.using "namespace mapnik::formatting"

-- * TextSymProperties


foreign import ccall "&hs_mapnik_destroy_TextSymProperties" destroyTextSymProperties :: FinalizerPtr TextSymProperties

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

withTextProperties :: Mapnik.TextProperties -> (Ptr TextProperties -> IO a) -> IO a
withTextProperties Mapnik.TextProperties{..} = bracket alloc dealloc . enter
  where
    alloc = [C.exp|text_properties_expressions * { new text_properties_expressions() }|]
    dealloc p = [C.exp|void{delete $(text_properties_expressions *p)}|]
    enter f p = f p --TODO

withFormatProperties :: Mapnik.TextFormatProperties -> (Ptr TextFormatProperties -> IO a) -> IO a
withFormatProperties Mapnik.TextFormatProperties{..} = bracket alloc dealloc . enter
  where
    alloc = [C.exp|format_properties * { new format_properties() }|]
    dealloc p = [C.exp|void{delete $(format_properties *p)}|]
    enter f p = f p --TODO

withLayoutProperties :: Mapnik.TextLayoutProperties -> (Ptr TextLayoutProperties -> IO a) -> IO a
withLayoutProperties Mapnik.TextLayoutProperties{..} = bracket alloc dealloc . enter
  where
    alloc = [C.exp|text_layout_properties * { new text_layout_properties() }|]
    dealloc p = [C.exp|void{delete $(text_layout_properties *p)}|]
    enter f p = f p --TODO

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

--TODO
unCreate :: TextSymProperties -> IO Mapnik.TextSymProperties
unCreate = const (return Mapnik.def)
