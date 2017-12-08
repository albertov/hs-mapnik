{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Mapnik.Bindings.VectorTileSpec (main, spec) where

import Mapnik.Bindings (registerDefaults, queryBox, Box(..), (.=))
import           Mapnik.Bindings.VectorTile
import qualified Mapnik.Bindings.Map as Map
import qualified Mapnik.Bindings.Datasource as Datasource
import qualified Mapnik.Bindings.Layer as Layer

import           Data.Text (Text)
import           Test.Hspec
import           System.FilePath ((</>))


main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll_ registerDefaults $ parallel $ do

  describe "render" $ do
    {-
    it "renders an empty Map" $ do
      m <- Map.create
      let xyz = XYZ 0 0 0
      Right tile <- render m (renderSettings xyz)
      Right srcs <- layerDatasources tile xyz
      length srcs `shouldBe` 0
      -}

    it "renders map with one layer" $ do
      m <- Map.create
      Map.setSrs m merc
      l <- Layer.create "Populated places"
      Layer.setSrs l merc
      Layer.setDatasource l =<< Datasource.create
        [ "type"     .= ("csv" :: String)
        , "file"     .= ("spec"</>"wkt.csv")
        ]
      Map.addLayer m l
      let xyz = XYZ 131072 131071 18
      Right tile <- render m (renderSettings xyz)
      eSources <- layerDatasources tile xyz
      let Right [("Populated places", ds)] = eSources
      let theBox = Box (-20026376.39) (-20048966.10) 20026376.39 20048966.10
      (_,feats) <- Datasource.features ds (queryBox theBox)
      length feats `shouldBe` 11

merc :: Text
merc = "+init=epsg:3857"
