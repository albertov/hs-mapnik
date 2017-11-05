{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.Rule (
  Rule
, unsafeNew
, create
, appendSymbolizer
, setName
, setFilter
, setMinScale
, setMaxScale
, setElse
, setAlso
) where

import           Mapnik.Bindings
import           Mapnik.Bindings.Util
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)

import qualified Language.C.Inline.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/rule.hpp>"
C.include "<mapnik/symbolizer.hpp>"

C.using "namespace mapnik"

--
-- * Rule


foreign import ccall "&hs_mapnik_destroy_Rule" destroyRule :: FinalizerPtr Rule

unsafeNew :: (Ptr (Ptr Rule) -> IO ()) -> IO Rule
unsafeNew = mkUnsafeNew Rule destroyRule

create :: IO Rule
create  = unsafeNew $ \p ->
  [C.block|void {*$(rule** p) = new rule();}|]

setName :: Rule -> Text -> IO ()
setName l (encodeUtf8 -> s) =
  [C.block|void { $fptr-ptr:(rule *l)->set_name(std::string($bs-ptr:s, $bs-len:s)); }|]

setMinScale :: Rule -> Double -> IO ()
setMinScale l (realToFrac -> s) =
  [C.block|void { $fptr-ptr:(rule *l)->set_min_scale($(double s)); }|]

setMaxScale :: Rule -> Double -> IO ()
setMaxScale l (realToFrac -> s) =
  [C.block|void { $fptr-ptr:(rule *l)->set_max_scale($(double s)); }|]

appendSymbolizer :: Rule -> Symbolizer -> IO ()
appendSymbolizer l s = [C.block| void {
  symbolizer sym(*$fptr-ptr:(symbolizer *s));
  $fptr-ptr:(rule *l)->append(std::move(sym));
  }|]

setFilter :: Rule -> Expression -> IO ()
setFilter l s = [C.block| void {
  expression_ptr const& filter(*$fptr-ptr:(expression_ptr *s));
  $fptr-ptr:(rule *l)->set_filter(filter);
  }|]

setElse :: Rule -> Bool -> IO ()
setElse r (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(rule *r)->set_else($(int q)); }|]

setAlso :: Rule -> Bool -> IO ()
setAlso r (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(rule *r)->set_also($(int q)); }|]


