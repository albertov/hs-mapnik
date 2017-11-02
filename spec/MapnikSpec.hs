{-# LANGUAGE OverloadedLists #-}
module MapnikSpec (main, spec) where

import qualified Data.ByteString as BS
import           Test.Hspec
import           Mapnik
import           Mapnik.Map as Map
import           Mapnik.Image as Image
import           Mapnik.Layer as Layer
import           Mapnik.Datasource as Datasource
import           Control.Monad (void)
import           Data.String (fromString)

main :: IO ()
main = hspec spec

isPng :: BS.ByteString -> Bool
isPng s = BS.take 6 s == fromString "\137PNG\r\n"

spec :: Spec
spec = beforeAll_ Mapnik.registerDefaults $ do

  it "renders map.xml as PNG" $ do
    m <- Map.create 512 512
    loadFixture m
    img <- Map.render m 1
    -- BS.writeFile "map.webp" (Image.serialize "webp" img)
    Image.serialize "png8" img `shouldSatisfy` isPng

  it "throws on broken XML" $ do
    m <- Map.create 512 512
    loadFixtureFrom "spec/bad.xml" m `shouldThrow` cppStdException

  it "can add layer and render" $ do
    m <- Map.create 11 11
    loadFixture m

    l <- Layer.create "Populated places"
    Layer.setSrs l "+proj=lcc +ellps=GRS80 +lat_0=49 +lon_0=-95 +lat+1=49 +lat_2=77 +datum=NAD83 +units=m +no_defs"
    Layer.addStyle l "popplaces"
    Layer.setDatasource l =<< Datasource.create
      [ "type"     .= "shape"
      , "encoding" .= "latin1"
      , "file"     .= "spec/data/popplaces"
      ]
    Map.addLayer m l
    void $ Map.render m 1

  it "throws on invalid datasource" $
    Datasource.create ["type".="shapes"] `shouldThrow` cppStdException

  it "can convert image to rgba8 data and read it back" $ do
    m <- Map.create 10 10
    loadFixture m
    img <- Map.render m 1
    let rgba8 = Image.toRgba8 img
        Just img2  = Image.fromRgba8 10 10 rgba8
    BS.length rgba8  `shouldBe` (10*10*4)
    --BS.writeFile "map.webp" (Image.serialize "webp" img2)
    Image.serialize "png8" img `shouldBe` Image.serialize "png8" img2

  it "can resize" $ do
    m <- Map.create 10 10
    loadFixture m
    img <- Map.render m 1
    BS.length (toRgba8 img) `shouldBe` (10*10*4)
    Map.resize m 300 200
    img2 <- Map.render m 1
    BS.length (toRgba8 img2) `shouldBe` (300*200*4)

loadFixture :: Map -> IO ()
loadFixture m = do
  loadFixtureFrom "spec/map.xml" m
  Map.setSrs m merc
  Map.zoomToBox m box

loadFixtureFrom :: String -> Map -> IO ()
loadFixtureFrom p m = Map.loadXml m =<< BS.readFile p

box :: Box
box = Box (-8024477.28459) 5445190.38849 (-7381388.20071) 5662941.44855

merc :: String
merc = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs +over"

cppStdException :: Selector CppException
cppStdException (CppStdException _) = True
cppStdException _ = False
