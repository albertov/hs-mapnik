{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mapnik.Bindings.Style (
  Style
, unsafeNew
, create
, getOpacity
, setOpacity
, setImageFiltersInflate
, getImageFiltersInflate
, addRule
, getRules
) where

import           Mapnik.Bindings.Types
import qualified Mapnik.Bindings.Rule as Rule

import           Control.Monad ((<=<))
import           Data.IORef
import           Foreign.ForeignPtr (FinalizerPtr, newForeignPtr)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (poke)

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU


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
  [CU.block|void {*$(feature_type_style** p) = new feature_type_style();}|]

getOpacity :: Style -> IO Double
getOpacity s = realToFrac <$> [CU.exp| float { $fptr-ptr:(feature_type_style *s)->get_opacity() }|]

setOpacity :: Style -> Double -> IO ()
setOpacity s (realToFrac -> v) =
  [CU.block|void { $fptr-ptr:(feature_type_style *s)->set_opacity($(double v)); }|]

getImageFiltersInflate :: Style -> IO Bool
getImageFiltersInflate s = toEnum . fromIntegral <$> [CU.exp|int { $fptr-ptr:(feature_type_style *s)->image_filters_inflate() }|]


setImageFiltersInflate :: Style -> Bool -> IO ()
setImageFiltersInflate l (fromIntegral . fromEnum -> q) =
  [CU.block|void { $fptr-ptr:(feature_type_style *l)->set_image_filters_inflate($(int q)); }|]

addRule :: Style -> Rule -> IO ()
addRule s r = [CU.block| void {
  rule rule(*$fptr-ptr:(rule *r));
  $fptr-ptr:(feature_type_style *s)->add_rule(std::move(rule));
  }|]

getRules :: Style -> IO [Rule]
getRules s = do
  rulesRef <- newIORef []
  let callback :: Ptr Rule -> IO ()
      callback ptr = do
        rule <- Rule.unsafeNew (`poke` ptr)
        modifyIORef' rulesRef (rule:)
  [C.block|void {
  typedef std::vector<rule> rule_list;
  rule_list const& rules = $fptr-ptr:(feature_type_style *s)->get_rules();
  for (rule_list::const_iterator it=rules.begin(); it!=rules.end(); ++it) {
    $fun:(void (*callback)(rule *))(new rule(*it));
  }
  }|]
  reverse <$> readIORef rulesRef
