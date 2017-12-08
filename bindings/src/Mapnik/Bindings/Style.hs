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
, getFilterMode
, setFilterMode
, getFilters
, setFilters
, getDirectFilters
, setDirectFilters
, getCompOp
, setCompOp
, setImageFiltersInflate
, getImageFiltersInflate
, addRule
, getRules
) where

import           Mapnik.Enums
import           Mapnik.ImageFilter as ImageFilter
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import qualified Mapnik.Bindings.Rule as Rule
import qualified Mapnik.Bindings.Cpp as C

import           Control.Exception
import           Data.IORef
import           Data.Text.Encoding (encodeUtf8)
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (poke)



C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/feature_type_style.hpp>"
C.include "<mapnik/rule.hpp>"
C.include "util.hpp"

C.using "namespace mapnik"

--
-- * Style


foreign import ccall "&hs_mapnik_destroy_Style" destroyStyle :: FinalizerPtr Style

unsafeNew :: (Ptr (Ptr Style) -> IO ()) -> IO Style
unsafeNew = mkUnsafeNew Style destroyStyle

create :: IO Style
create  = unsafeNew $ \p ->
  [C.block|void {*$(feature_type_style** p) = new feature_type_style();}|]

getOpacity :: Style -> IO Double
getOpacity s = realToFrac <$> [C.exp| float { $fptr-ptr:(feature_type_style *s)->get_opacity() }|]

setOpacity :: Style -> Double -> IO ()
setOpacity s (realToFrac -> v) =
  [C.block|void { $fptr-ptr:(feature_type_style *s)->set_opacity($(double v)); }|]

getImageFiltersInflate :: Style -> IO Bool
getImageFiltersInflate s = toEnum . fromIntegral <$> [C.exp|int { $fptr-ptr:(feature_type_style *s)->image_filters_inflate() }|]


setImageFiltersInflate :: Style -> Bool -> IO ()
setImageFiltersInflate l (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(feature_type_style *l)->set_image_filters_inflate($(int q)); }|]

addRule :: Style -> Rule -> IO ()
addRule s r = [C.block| void {
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
  [C.safeBlock|void {
  auto const& rules = $fptr-ptr:(feature_type_style *s)->get_rules();
  for (auto it=rules.begin(); it!=rules.end(); ++it) {
    $fun:(void (*callback)(rule *))(new rule(*it));
  }
  }|]
  reverse <$> readIORef rulesRef

getFilterMode :: Style -> IO (Maybe FilterMode)
getFilterMode s = fmap (fmap (toEnum . fromIntegral)) $
  newMaybe $ \(has,p) -> [C.block|void {
  auto const& ret = $fptr-ptr:(feature_type_style *s)->get_filter_mode();
  static const feature_type_style dfl;
  *$(int *p) = static_cast<int>(ret);
  *$(int *has) = ret != dfl.get_filter_mode();
  }|]

setFilterMode :: Style -> FilterMode -> IO ()
setFilterMode s (fromIntegral . fromEnum -> v) = [C.block|void{
  $fptr-ptr:(feature_type_style *s)->set_filter_mode(
    static_cast<filter_mode_enum>($(int v)));
  }|]

getFilters :: Style -> IO [ImageFilter]
getFilters s = do
  mFs <- newTextMaybe "Style.getFilters" $ \(p,len) -> [C.block|void{
    auto const& style = *$fptr-ptr:(feature_type_style *s);
    if (style.image_filters().size() > 0) {
      std::string s;
      std::back_insert_iterator<std::string> sink(s);
      generate_image_filters(sink, style.image_filters());
      mallocedString(s, $(char **p), $(int *len));
    } else {
      *$(char **p) = nullptr;
    }
  }|]
  case mFs of
    Just fs -> case ImageFilter.parseMany fs of
      Right r -> return r
      Left e -> throwIO (InternalError ("getFilters: "++show e))
    Nothing -> return []

setFilters :: Style -> [ImageFilter] -> IO ()
setFilters s (encodeUtf8 . ImageFilter.toTextMany -> fs) = [C.block|void{
  auto style = $fptr-ptr:(feature_type_style *s);
  parse_image_filters( std::string($bs-ptr:fs, $bs-len:fs)
                     , style->image_filters());
  }|]

getDirectFilters :: Style -> IO [ImageFilter]
getDirectFilters s = do
  mFs <- newTextMaybe "Style.getDirectFilters" $ \(p,len) -> [C.block|void{
    auto const& style = *$fptr-ptr:(feature_type_style *s);
    if (style.direct_image_filters().size() > 0) {
      std::string s;
      std::back_insert_iterator<std::string> sink(s);
      generate_image_filters(sink, style.direct_image_filters());
      mallocedString(s, $(char **p), $(int *len));
    } else {
      *$(char **p) = nullptr;
    }
  }|]
  case mFs of
    Just fs -> case ImageFilter.parseMany fs of
      Right r -> return r
      Left e -> throwIO (InternalError ("getDirectFilters: "++show e))
    Nothing -> return []

setDirectFilters :: Style -> [ImageFilter] -> IO ()
setDirectFilters s (encodeUtf8 . ImageFilter.toTextMany -> fs) = [C.block|void{
  auto style = $fptr-ptr:(feature_type_style *s);
  parse_image_filters( std::string($bs-ptr:fs, $bs-len:fs)
                     , style->direct_image_filters());
  }|]

getCompOp :: Style -> IO (Maybe CompositeMode)
getCompOp s = fmap (fmap (toEnum . fromIntegral)) $
  newMaybe $ \(has,p) -> [C.block|void {
  auto const& ret = $fptr-ptr:(feature_type_style *s)->comp_op();
  if (ret) {
    *$(int *has) = 1;
    *$(int *p) = static_cast<int>(*ret);
  } else {
    *$(int *has) = 0;
  }
  }|]

setCompOp :: Style -> CompositeMode -> IO ()
setCompOp s (fromIntegral . fromEnum -> v) = [C.block|void{
  $fptr-ptr:(feature_type_style *s)->set_comp_op(
    static_cast<composite_mode_e>($(int v)));
  }|]
