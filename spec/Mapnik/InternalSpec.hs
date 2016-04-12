{-# LANGUAGE OverloadedStrings #-}
module Mapnik.InternalSpec (main, spec) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import           Test.Hspec
import           Mapnik.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll_ register_defaults $ do

  it "renders map.xml as PNG" $ do
    img <- runMapnik_ $ do
      m <- createMap 800 600
      set_srs m merc
      load_map_string m =<< liftIO (BS.readFile "spec/map.xml")
      zoom_to_box m box
      render_to_image m
    BS.take 6 (serialize_image PNG img) `shouldBe` "\137PNG\r\n"
    where
      merc = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs +over"
      box = bbox (-8024477.28459) 5445190.38849 (-7381388.20071) 5662941.44855
