{-# LANGUAGE OverloadedStrings #-}
module Mapnik.InternalSpec (main, spec) where

import qualified Data.ByteString as BS
import           Test.Hspec
import           Mapnik

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll_ register_defaults $ do

  it "renders map.xml as PNG" $ do
    m <- createMap 512 512
    loadFixture m
    set_srs m merc
    zoom_to_box m box
    img <- render_to_image m 1
    -- BS.writeFile "map.webp" (serialize_image "webp" img)
    BS.take 6 (serialize_image "png8" img) `shouldBe` "\137PNG\r\n"

  it "throws on broken XML" $ do
    m <- createMap 512 512
    loadFixtureFrom "spec/bad.xml" m `shouldThrow` anyException

  it "can convert image to rgba8 data and read it back" $ do
    m <- createMap 10 10
    loadFixture m
    set_srs m merc
    zoom_to_box m box
    img <- render_to_image m 1
    let rgba8 = image_to_rgba8 img
        Just img2  = image_from_rgba8 10 10 rgba8
    BS.length rgba8  `shouldBe` (10*10*4)
    BS.take 6 (serialize_image "png8" img2) `shouldBe` "\137PNG\r\n"
    --BS.writeFile "map.webp" (serialize_image "webp" img2)
    serialize_image "png8" img `shouldBe` serialize_image "png8" img2

  it "can resize" $ do
    m <- createMap 10 10
    loadFixture m
    resize m 512 512
    img <- render_to_image m 1
    BS.take 6 (serialize_image "png8" img) `shouldBe` "\137PNG\r\n"

loadFixture :: Map -> IO ()
loadFixture = loadFixtureFrom "spec/map.xml"

loadFixtureFrom :: String -> Map -> IO ()
loadFixtureFrom p m = load_map_string m =<< BS.readFile p

box :: Box
box = Box (-8024477.28459) 5445190.38849 (-7381388.20071) 5662941.44855

merc :: String
merc = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs +over"
