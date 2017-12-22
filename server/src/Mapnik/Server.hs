-- TODO: Move to hs-mapnik-server
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module Mapnik.Server (Settings(..), ogcServer) where

import qualified Mapnik.Bindings as Mapnik
import Mapnik.Bindings.Render as RR (renderSettings, render)
import Mapnik.Bindings.VectorTile.Render as VR (renderSettings, render)
import qualified Mapnik.Bindings.Types  as MapnikB (Map(Map))

import Control.Lens
import Control.Monad.Reader     ( MonadReader )
import Control.Monad.Logger     ( MonadLogger, logDebug, runLoggingT
                                , MonadLoggerIO(askLoggerIO)
                                , logInfo, logDebug)
import Control.Monad.Base       (MonadBase(liftBase))
import Control.Monad.Trans.Control (MonadBaseControl)
import Codec.MIME.Type          (Type(..), MIMEType(..), showType)
import Data.Time                (NominalDiffTime)
import Data.Maybe               (fromJust)
import Data.Scientific          (Scientific)
import qualified Data.Vector.Storable as V
import qualified Data.Pool as P
import Protolude hiding (Map, hPutStrLn, replace, to)
import Foreign.ForeignPtr (finalizeForeignPtr)
import Network.Wai as Wai (
    Application
  , Request(..)
  , responseLBS
  , Response
  , ResponseReceived
  )
import Network.Wai.Ogc.Common (parseRequest, ParseError(..))
import Network.Wai.Ogc.Wms (SomeRequest(..), Request(..))
import qualified Network.Wai.Ogc.Wms as Wms
import Network.HTTP.Types (status200, status415, status400)
import GHC.Exts (fromList)
import Prelude (String)



data Settings = Settings
 { poolStripes :: Int
 , poolTimeout :: NominalDiffTime
 , poolSize :: Int
 }

data OgcEnv = OgcEnv
  { settings :: Settings
  , theMap :: Mapnik.Map
  , mapPool  :: P.Pool MapnikB.Map
  }

type LiftedApplication r m = r -> (Response -> m ResponseReceived) -> m ResponseReceived

-- TODO: Move to bindings
finalizeMap :: MapnikB.Map -> IO ()
finalizeMap (MapnikB.Map fp) = finalizeForeignPtr fp

ogcServer
  :: (MonadLoggerIO m, MonadBaseControl IO m)
  => Settings
  -> Mapnik.Map
  -> m Application
ogcServer cfg m = do
  logger_ <- askLoggerIO
  $logInfo "Creating map pool for Mapnik OGCServer"
  app logger_ <$> liftBase
    (P.createPool
      (runLoggingT createMap_ logger_)
      (flip runLoggingT logger_ . finalizeMap_)
      (poolStripes cfg)
      (poolTimeout cfg)
      (poolSize cfg)
      )
  where
  createMap_   = do
    $logDebug "Map pool: Creating map"
    liftBase $ Mapnik.toMapnik m
  finalizeMap_ m' = do
    $logDebug "Map pool: destroying map"
    liftBase (finalizeMap m')
  app logger_ p req ( (liftBase .) -> resp ) = flip runLoggingT logger_ $
    let env = OgcEnv { settings = cfg, theMap = m, mapPool = p }
    in case parseRequest (queryString req) Nothing of
      Right (SomeRequest wmsReq) ->
        runReaderT (dispatchWms wmsReq resp)  env
      Left  err ->
        runReaderT (renderError err req resp) env

withMap :: (MonadReader OgcEnv m, MonadBaseControl IO m) => (MapnikB.Map -> m a) -> m a
withMap f = asks mapPool >>= (`P.withResource` f)

dispatchWms
  :: (MonadLogger m, MonadReader OgcEnv m, MonadBaseControl IO m)
  => LiftedApplication (Wms.Request t) m
dispatchWms GetMap
  { wmsMapLayers
  , wmsMapCrs
  , wmsMapBbox
  , wmsMapSize
  , wmsMapFormat
  , wmsMapTransparent
  , wmsMapBackground
  --, wmsMapExceptions
  , wmsMapTime
  , wmsMapElevation
  , wmsMapDimensions
  } respond =
    case toMapnikFormatString wmsMapFormat of
      Nothing -> unsupportedFormat
      Just fmt ->
        case toProj4 wmsMapCrs of
          Nothing -> unsupportedCrs fmt
          Just srs -> do
            img <- withMap $ \m ->
              liftBase $ RR.render m $ cfg
                 & Mapnik.srs    .~ Just srs
                 & Mapnik.layers ?~ toActiveLayers wmsMapLayers
                 & Mapnik.backgroundColor .~ toMapnikBgColor wmsMapTransparent wmsMapBackground
                 & Mapnik.variables .~ fromList (map dimToVariable allDims)
            bs <- maybe (panic "invalid format string returned by toFormatString")
                        return (Mapnik.serialize fmt img)
            respond $ responseLBS
              status200 [("content-type", toS (showType wmsMapFormat))] (toS bs)
  where
    allDims = catMaybes ( fmap toTimeDim wmsMapTime
                        : fmap toElevDim wmsMapElevation
                        : map Just wmsMapDimensions)
    width_ = Wms.width wmsMapSize
    height_ = Wms.height wmsMapSize
    cfg = RR.renderSettings width_ height_ box
        & Mapnik.aspectFixMode .~ Mapnik.Respect
    box = Mapnik.Box (realToFrac (Wms.minx wmsMapBbox))
                     (realToFrac (Wms.miny wmsMapBbox))
                     (realToFrac (Wms.maxx wmsMapBbox))
                     (realToFrac (Wms.maxy wmsMapBbox))

    mkBlankImg fmt = 
      let blankImg = fromJust $ Mapnik.fromRgba8 $
            Mapnik.ImageRgba8
              (width_, height_)
              (V.replicate (width_*height_) (Mapnik.PixelRgba8 0))
      in toS (fromJust (Mapnik.serialize fmt blankImg))

    -- TODO: Implement ExcInImage and ExcXml
    unsupportedCrs fmt = do
      $logDebug $ "Unsupported CRS: " <> show wmsMapCrs
      respond $ responseLBS
        status400 [("content-type", toS (showType wmsMapFormat))] (mkBlankImg fmt)

    unsupportedFormat = do
      $logDebug $ "Unsupported Format: " <> show wmsMapFormat
      respond $ responseLBS status415 [("content-type", "text/plain")] ""

dimToVariable :: Wms.Dimension -> (Text, Mapnik.Value)
dimToVariable (Wms.Dimension k n) = (k, Mapnik.TextValue (toS n))

toTimeDim :: Wms.Time  -> Wms.Dimension
toTimeDim (Wms.Time (Wms.TimeStamp tz)) = Wms.Dimension "TIME" (show tz)
toTimeDim (Wms.Time Wms.Current       ) = Wms.Dimension "TIME" "CURRENT"
--FIXME: Handle intervals properly
toTimeDim (Wms.Interval b _ _) = toTimeDim (Wms.Time b)

toElevDim :: Scientific -> Wms.Dimension
toElevDim = Wms.Dimension "ELEVATION" . show

toMapnikBgColor :: Maybe Wms.Transparent -> Maybe Wms.BgColor -> Maybe Mapnik.Color
toMapnikBgColor Nothing Nothing = Nothing
toMapnikBgColor (fromMaybe Wms.Transparent -> t) (Just (Wms.BgColor r g b)) =
  Just (Mapnik.RGBA r g b (case t of {Wms.Opaque -> 255; Wms.Transparent  -> 128}))
toMapnikBgColor (Just Wms.Transparent) Nothing = Nothing
toMapnikBgColor (Just Wms.Opaque) Nothing = Just (Mapnik.RGBA 0 0 0 255)
    

-- Ignores styles
toActiveLayers :: [Wms.Layer] -> [Text]
toActiveLayers = map $ \(Wms.Layer name _) -> Wms.unName name

toMapnikFormatString :: Type -> Maybe String
toMapnikFormatString = \case
  Type (Image "png")  _ -> Just "png8"
  Type (Image "webp") _ -> Just "webp"
  Type (Image "jpeg") _ -> Just "jpeg75"
  _                     -> Nothing
  
toProj4 :: Wms.Crs -> Maybe Text
toProj4 (Wms.Proj4 s) = Just (toS s)
toProj4 (Wms.Epsg  s) = Just ("+init=epsg:" <> show s)
toProj4 _             = Nothing  


-- | FIXME: Should honor what wmsMap
renderError
  :: (MonadLogger m, MonadReader OgcEnv m, MonadBaseControl IO m)
  => ParseError -> LiftedApplication Wai.Request m
renderError err _ respond = do
  $logDebug $ "Invalid WMS Request: " <> show err
  respond $ responseLBS status400 [("content-type", "text/plain")] (show err)

