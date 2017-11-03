{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.Style (
  Style
, unsafeNew
, create
, setOpacity
, setImageFiltersInflate
, addRule
) where

import           Mapnik.Bindings
import           Control.Monad ((<=<))
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/feature_type_style.hpp>"
C.include "<mapnik/rule.hpp>"

C.using "namespace mapnik"

--
-- * Style


foreign import ccall "&hs_mapnik_destroy_Style" destroyStyle :: FinalizerPtr Style

unsafeNew :: (Ptr (Ptr Style) -> IO ()) -> IO Style
unsafeNew = fmap Style . newForeignPtr destroyStyle <=< C.withPtr_

create :: IO Style
create  = unsafeNew $ \p ->
  [C.block|void {*$(feature_type_style** p) = new feature_type_style();}|]

setOpacity :: Style -> Double -> IO ()
setOpacity l (realToFrac -> s) =
  [C.block|void { $fptr-ptr:(feature_type_style *l)->set_opacity($(double s)); }|]

setImageFiltersInflate :: Style -> Bool -> IO ()
setImageFiltersInflate l (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(feature_type_style *l)->set_image_filters_inflate($(int q)); }|]

addRule :: Style -> Rule -> IO ()
addRule s r = [C.block| void {
  rule rule(*$fptr-ptr:(rule *r));
  $fptr-ptr:(feature_type_style *s)->add_rule(std::move(rule));
  }|]

