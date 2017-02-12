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
      m <- createMap 512 512
      loadFixture m
      set_srs m merc
      zoom_to_box m box
      render_to_image m 1
    --BS.writeFile "map.png" (serialize_image PNG img)
    BS.take 6 (serialize_image "png8" img) `shouldBe` "\137PNG\r\n"

  it "can convert image to rgba8 data and read it back" $ do
    img <- runMapnik_ $ do
      m <- createMap 512 512
      loadFixture m
      set_srs m merc
      zoom_to_box m box
      render_to_image m 1
    let rgba8 = image_to_rgba8 img
        Just img2  = image_from_rgba8 512 512 rgba8
    BS.length rgba8  `shouldBe` (512*512*4)
    BS.take 6 (serialize_image "png8" img2) `shouldBe` "\137PNG\r\n"

  it "can resize" $ do
    img <- runMapnik_ $ do
      m <- createMap 10 10
      loadFixture m
      resize m 512 512
      render_to_image m 1
    BS.take 6 (serialize_image "png8" img) `shouldBe` "\137PNG\r\n"

loadFixture :: Map -> MapnikM ()
loadFixture m =
  load_map_string m =<< liftIO (BS.readFile "spec/map.xml")


box :: BBox
box = bbox (-8024477.28459) 5445190.38849 (-7381388.20071) 5662941.44855

merc :: String
merc = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs +over"
