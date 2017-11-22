{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.Bindings.Layer (
  Layer
, unsafeNew
, create
, addStyle
, getStyles
, getDatasource
, setDatasource
, getName
, setName
, getSrs
, setSrs
, setBufferSize
, getBufferSize
, getMaxExtent
, setMaxExtent
, getMaxScaleDenominator
, setMaxScaleDenominator
, getMinScaleDenominator
, setMinScaleDenominator
, getQueryable
, setQueryable
, getGroupBy
, setGroupBy
, getClearLabelCache
, setClearLabelCache
, getCacheFeatures
, setCacheFeatures
) where

import           Mapnik (StyleName)
import           Mapnik.Bindings.Types
import           Mapnik.Bindings.Util
import           Mapnik.Bindings.Orphans()
import qualified Mapnik.Bindings.Datasource as Datasource
import           Data.IORef
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.ByteString.Unsafe (unsafePackMallocCStringLen)
import           Foreign.ForeignPtr (FinalizerPtr)
import           Foreign.Marshal.Utils (with)
import           Foreign.C.String (CString)
import           Foreign.Ptr (Ptr)

import qualified Mapnik.Bindings.Cpp as C


C.context mapnikCtx

C.include "<string>"
C.include "<mapnik/layer.hpp>"
C.include "<mapnik/datasource.hpp>"
C.include "util.hpp"

C.using "namespace mapnik"
C.using "bbox = box2d<double>"

--
-- * Layer


foreign import ccall "&hs_mapnik_destroy_Layer" destroyLayer :: FinalizerPtr Layer

unsafeNew :: (Ptr (Ptr Layer) -> IO ()) -> IO Layer
unsafeNew = mkUnsafeNew Layer destroyLayer

create :: Text -> IO Layer
create (encodeUtf8 -> name) =
  unsafeNew $ \p -> [C.block|void {*$(layer** p) = new layer(std::string($bs-ptr:name, $bs-len:name));}|]

addStyle :: Layer -> Text -> IO ()
addStyle l (encodeUtf8 -> s) = [C.block| void {
  $fptr-ptr:(layer *l)->add_style(std::string($bs-ptr:s, $bs-len:s));
  }|]

getStyles :: Layer -> IO [StyleName]
getStyles l = do
  stylesRef <- newIORef []
  let callback :: CString -> C.CInt -> IO ()
      callback ptr (fromIntegral -> len) = do
        styleName <- unsafePackMallocCStringLen (ptr, len)
        modifyIORef' stylesRef (styleName:)
  [C.safeBlock|void {
  using style_list = std::vector<std::string>;
  style_list const& styles = $fptr-ptr:(layer *l)->styles();
  for (style_list::const_iterator it=styles.begin(); it!=styles.end(); ++it) {
    char *key;
    int len;
    mallocedString(*it, &key, &len);
    $fun:(void (*callback)(char*, int))(key, len);
  }
  }|]
  reverse <$> (mapM (decodeUtf8Ctx "Layer.getStyles") =<< readIORef stylesRef)

getGroupBy :: Layer -> IO Text
getGroupBy l = newText "Layer.getGroupBy" $ \(p,len) -> [C.block|void {
  std::string const &srs = $fptr-ptr:(layer *l)->group_by();
  mallocedString(srs, $(char **p), $(int *len));
  }|]

setGroupBy :: Layer -> Text -> IO ()
setGroupBy l (encodeUtf8 -> srs) =
  [C.block|void { $fptr-ptr:(layer *l)->set_group_by(std::string($bs-ptr:srs, $bs-len:srs)); }|]

getName :: Layer -> IO Text
getName l = newText "Layer.getName" $ \(p,len) -> [C.block|void {
  std::string const &name = $fptr-ptr:(layer *l)->name();
  mallocedString(name, $(char **p), $(int *len));
  }|]

setName :: Layer -> Text -> IO ()
setName l (encodeUtf8 -> v) =
  [C.block|void { $fptr-ptr:(layer *l)->set_name(std::string($bs-ptr:v, $bs-len:v)); }|]

getSrs :: Layer -> IO Text
getSrs l = newText "Layer.getSrs" $ \(p,len) -> [C.block|void {
  std::string const &srs = $fptr-ptr:(layer *l)->srs();
  mallocedString(srs, $(char **p), $(int *len));
  }|]

setSrs :: Layer -> Text -> IO ()
setSrs l (encodeUtf8 -> srs) =
  [C.block|void { $fptr-ptr:(layer *l)->set_srs(std::string($bs-ptr:srs, $bs-len:srs)); }|]

getBufferSize :: Layer -> IO (Maybe Int)
getBufferSize l = do
  (has, fromIntegral -> ret) <- C.withPtrs_ $ \(has,ret) ->
    [C.block|void {
      boost::optional<int> ret = $fptr-ptr:(layer *l)->buffer_size();
      if (ret) {
        *$(int *has) = 1;
        *$(int *ret) = *ret;
      } else {
        *$(int *has) = 0;
      }
   }|]
  return $ if has == 1 then Just ret else Nothing

setBufferSize :: Layer -> Int -> IO ()
setBufferSize l (fromIntegral -> s) =
  [C.block|void { $fptr-ptr:(layer *l)->set_buffer_size($(int s)); }|]

getMaxScaleDenominator :: Layer -> IO Double
getMaxScaleDenominator l = realToFrac <$> [C.exp|double {$fptr-ptr:(layer *l)->maximum_scale_denominator()}|]

setMaxScaleDenominator :: Layer -> Double -> IO ()
setMaxScaleDenominator l (realToFrac -> s) =
  [C.block|void { $fptr-ptr:(layer *l)->set_maximum_scale_denominator($(double s)); }|]

getMinScaleDenominator :: Layer -> IO Double
getMinScaleDenominator l = realToFrac <$> [C.exp|double {$fptr-ptr:(layer *l)->minimum_scale_denominator()}|]

setMinScaleDenominator :: Layer -> Double -> IO ()
setMinScaleDenominator l (realToFrac -> s) =
  [C.block|void { $fptr-ptr:(layer *l)->set_minimum_scale_denominator($(double s)); }|]

getQueryable :: Layer -> IO Bool
getQueryable l = toEnum . fromIntegral <$> [C.exp|int { $fptr-ptr:(layer *l)->queryable() }|]

setQueryable :: Layer -> Bool -> IO ()
setQueryable l (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(layer *l)->set_queryable($(int q)); }|]

getClearLabelCache :: Layer -> IO Bool
getClearLabelCache l = toEnum . fromIntegral <$> [C.exp|int { $fptr-ptr:(layer *l)->clear_label_cache() }|]

setClearLabelCache :: Layer -> Bool -> IO ()
setClearLabelCache l (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(layer *l)->set_clear_label_cache($(int q)); }|]

setCacheFeatures :: Layer -> Bool -> IO ()
setCacheFeatures l (fromIntegral . fromEnum -> q) =
  [C.block|void { $fptr-ptr:(layer *l)->set_cache_features($(int q)); }|]

getCacheFeatures :: Layer -> IO Bool
getCacheFeatures l = toEnum . fromIntegral <$> [C.exp|int { $fptr-ptr:(layer *l)->cache_features() }|]

getDatasource :: Layer -> IO (Maybe Datasource)
getDatasource l = Datasource.unsafeNewMaybe $ \p ->
  [C.block|void {
    auto p = $fptr-ptr:(layer *l)->datasource();
    *$(datasource_ptr **p) = p ? new datasource_ptr(p) : NULL;
    }|]

setDatasource :: Layer -> Datasource -> IO ()
setDatasource l ds =
  [C.block|void { $fptr-ptr:(layer *l)->set_datasource(*$fptr-ptr:(datasource_ptr *ds)); }|]

getMaxExtent :: Layer -> IO (Maybe Box)
getMaxExtent l =  newMaybe $ \(has,p) ->
    [C.block|void {
      auto res = $fptr-ptr:(layer *l)->maximum_extent();
      if (res) {
        *$(bbox *p) = *res;
        *$(int *has) = 1;
      } else {
        *$(int *has) = 0;
      }
      }|]

setMaxExtent :: Layer -> Box -> IO ()
setMaxExtent l box = with box $ \boxPtr ->
  [C.block|void { $fptr-ptr:(layer *l)->set_maximum_extent(*$(bbox *boxPtr)); }|]
