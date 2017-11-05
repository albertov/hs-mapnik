{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Mapnik.BindingsSpec (main, spec) where

import qualified Data.ByteString as BS
import           Test.Hspec
import           Mapnik.Bindings
import           Mapnik.Bindings.Registry (registerDefaults)
import           Mapnik.Bindings.Map as Map
import           Mapnik.Bindings.Image as Image
import           Mapnik.Bindings.Layer as Layer
import           Mapnik.Bindings.Projection as Projection
import           Mapnik.Bindings.Rule as Rule
import           Mapnik.Bindings.Style as Style
import           Mapnik.Bindings.Expression as Expression
import           Mapnik.Bindings.Datasource as Datasource
import           Mapnik.Bindings.Color as Color
import           Control.Monad (void)
import           Data.Maybe (isJust, isNothing)
import           Data.List (lookup)
import           Data.Either (isLeft, isRight)
import           Data.String (fromString)

main :: IO ()
main = hspec spec

isPng :: BS.ByteString -> Bool
isPng s = BS.take 6 s == fromString "\137PNG\r\n"

spec :: Spec
spec = beforeAll_ registerDefaults $ do

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
      [ "type"     .= ("shape" :: String)
      , "encoding" .= ("latin1" :: String)
      , "file"     .= ("spec/data/popplaces" :: String)
      ]
    Map.addLayer m l
    void $ Map.render m 1

  it "throws on invalid datasource" $
    Datasource.create ["type".= ("shapes" :: String)] `shouldThrow` cppStdException

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

  it "throws on invalid size" $ do
    m <- Map.create (-1) (-1)
    loadFixture m
    Map.render m 1 `shouldThrow` cppStdException

  it "can create valid projection" $ do
    fromProj4 merc `shouldSatisfy` isRight

  it "can show valid projection" $ do
    show (fromProj4 merc) `shouldBe` "Right +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs +over"


  it "cannot create invalid projection" $ do
    fromProj4 "foo" `shouldSatisfy` isLeft

  it "can trasform" $ do
    let Right src = fromProj4 merc
        Right dst = fromProj4 "+proj=lcc +ellps=GRS80 +lat_0=49 +lon_0=-95 +lat+1=49 +lat_2=77 +datum=NAD83 +units=m +no_defs"
        trans = transform src dst
        expected = Box { minx = 1372637.1001942465
                       , miny = -247003.8133187965
                       , maxx = 1746737.6177269476
                       , maxy = -25098.59307479199
                       }
    forward trans box `shouldBe` expected

  it "can parse good color" $ do
    Color.create "rgba(3,4,5,1)" `shouldSatisfy` isJust
    Color.create "steelblue" `shouldSatisfy` isJust

  it "can show color" $ do
    show (Color.create "rgb(3,4,5)") `shouldBe` "Just rgb(3,4,5)"

  it "cannot parse bad color" $ do
    Color.create "rgba(3,4,5" `shouldSatisfy` isNothing
    Color.create "steelblu" `shouldSatisfy` isNothing

  it "cannot get unexistent layer" $ do
    m <- Map.create 512 512
    getLayer m 1 `shouldThrow` cppStdException

  it "can get Map styles" $ do
    m <- Map.create 512 512
    loadFixture m
    sts <- map fst <$> Map.getStyles m
    sts `shouldBe` ["drainage","highway-border","highway-fill","popplaces","provinces","provlines","road-border","road-fill","smallroads"]

  it "can get Style rules" $ do
    m <- Map.create 512 512
    loadFixture m
    Just style <- lookup "provinces" <$> Map.getStyles m
    rules <- Style.getRules style
    length rules `shouldBe` 2

  it "can get Map srs" $ do
    m <- Map.create 512 512
    loadFixture m
    Map.setSrs m merc
    srs <- Map.getSrs m
    srs `shouldBe` merc

  it "can get max extent when Nothing" $ do
    m <- Map.create 512 512
    e <- Map.getMaxExtent m
    e `shouldBe` Nothing

  it "can get max extent when Just" $ do
    m <- Map.create 512 512
    Map.setMaxExtent m box
    e <- Map.getMaxExtent m
    e `shouldBe` Just box

  it "getDatasource returns Nothing if no datasource" $ do
    l <- Layer.create "fooo"
    ds <- Layer.getDatasource l
    ds `shouldBe` Nothing

  it "getDatasource returns Just if datasource" $ do
    l <- Layer.create "fooo"
    Layer.setDatasource l =<< Datasource.create
      [ "type"     .= ("shape" :: String)
      , "encoding" .= ("latin1" :: String)
      , "file"     .= ("spec/data/popplaces" :: String)
      ]
    ds <- Layer.getDatasource l
    ds `shouldSatisfy` isJust

  it "can get Layer styles" $ do
    l <- Layer.create "fooo"
    sts <- Layer.getStyles l
    sts `shouldBe` []
    Layer.addStyle l "foo"
    Layer.addStyle l "bar"
    sts2 <- Layer.getStyles l
    sts2 `shouldBe` ["foo", "bar"]

  it "can create rule and set filter" $ do
    r <- Rule.create
    let Right e = Expression.parse "[foo]=bar"
    Rule.setFilter r e

loadFixture :: Map -> IO ()
loadFixture m = do
  loadFixtureFrom "spec/map.xml" m
  Map.setSrs m merc
  Map.zoomToBox m box

loadFixtureFrom :: String -> Map -> IO ()
loadFixtureFrom p m = Map.loadXml m =<< BS.readFile p

box :: Box
box = Box (-8024477.28459) 5445190.38849 (-7381388.20071) 5662941.44855

merc = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs +over"

cppStdException :: Selector CppException
cppStdException (CppStdException _) = True
cppStdException _ = False
