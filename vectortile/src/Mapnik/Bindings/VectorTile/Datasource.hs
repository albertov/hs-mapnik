{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
module Mapnik.Bindings.VectorTile.Datasource where

import           Mapnik.Bindings.VectorTile.Types
import           Mapnik.Bindings.Types (Datasource(..), mapnikCtx)
import qualified Mapnik.Bindings.Datasource as Datasource
import           Mapnik.Bindings.Util (decodeUtf8Keys)
import           Mapnik.Bindings (MapnikError)
import qualified Mapnik.Bindings.Cpp as C

import           Control.Exception
import           Data.IORef
import           Data.ByteString (packCStringLen)
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.C.Types
import           Foreign.C.String
import           Data.Text (Text)

C.context mapnikCtx
C.include "hs_vector_tile_datasource.hpp"
C.using "namespace mapnik"
C.using "namespace mapnik::vector_tile_impl"


type LayerName = Text

-- | Loads a 'Datasource' for features withing an 'XYZ' tileindex for each
-- layer in a 'MVTile'
layerDatasources
  :: MVTile
  -> XYZ
  -> IO (Either MapnikError [(LayerName, Datasource)])
layerDatasources (MVTile pbf) xyz = try $ do
  ref <- newIORef []
  let cb :: CString -> CInt -> Ptr Datasource -> IO ()
      cb name len p = do ds <- Datasource.unsafeNew (flip poke p)
                         name' <- packCStringLen (name, fromIntegral len)
                         modifyIORef' ref ((name',ds):)
  [C.catchBlock|
    auto const buf = std::make_shared<std::string>($bs-ptr:pbf, $bs-len:pbf);
    protozero::pbf_reader tile(*buf);
    while (tile.next(3)) { // Iterate all layers (msgid=3)
      protozero::pbf_reader layer = tile.get_message();
      auto ds = std::make_shared<hs_tile_datasource_pbf>(
        buf, layer, $(uint64_t x), $(uint64_t y), $(uint64_t z)
        );
      auto const& name = ds->get_name();
      $fun:(void(*cb)(char *,int, datasource_ptr*))(
        const_cast<char*>(name.data()), name.size(), new datasource_ptr(ds)
        );
    }
  |]
  reverse <$> (decodeUtf8Keys "layerDatasources" =<< readIORef ref)
  where XYZ (fromIntegral->x) (fromIntegral->y) (fromIntegral->z) = xyz
