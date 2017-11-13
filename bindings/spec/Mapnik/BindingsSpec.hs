{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Mapnik.BindingsSpec (main, spec) where

import qualified Data.ByteString as BS
import           Test.Hspec
import qualified Mapnik
import           Mapnik ( Color(..), Dash(..), DashArray, Prop(..))
import qualified Mapnik.Lens as L
import           Mapnik.Enums
import           Mapnik.Bindings
import           Mapnik.Bindings.Registry (registerDefaults)
import           Mapnik.Bindings.Map as Map
import           Mapnik.Bindings.Image as Image
import           Mapnik.Bindings.Layer as Layer
import           Mapnik.Bindings.Projection as Projection
import           Mapnik.Bindings.Rule as Rule
import           Mapnik.Bindings.Style as Style
import           Mapnik.Bindings.Transform as Transform
import           Mapnik.Bindings.Expression as Expression
import           Mapnik.Bindings.Datasource as Datasource
import           Mapnik.Bindings.Symbolizer as Symbolizer
import           Mapnik.Bindings.TextPlacements as TextPlacements
import           Mapnik.Bindings.FromMapnik
import           Control.Lens hiding ((.=))
import           Control.Monad (void)
import           Data.Text (Text)
import           Data.Maybe (isJust, isNothing)
import           Data.List (lookup)
import           Data.Either (isLeft, isRight)

main :: IO ()
main = hspec spec

isPng :: BS.ByteString -> Bool
isPng s = BS.take 6 s == "\137PNG\r\n"

spec :: Spec
spec = beforeAll_ registerDefaults $ do

  describe "Map" $ do
    it "renders as PNG" $ do
      m <- Map.create 512 512
      loadFixture m
      img <- Map.render m 1
      -- BS.writeFile "map.webp" (Image.serialize "webp" img)
      let Just bs = Image.serialize "png8" img
      bs `shouldSatisfy` isPng


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

    it "can get styles" $ do
      m <- Map.create 512 512
      loadFixture m
      sts <- map fst <$> Map.getStyles m
      sts `shouldMatchList` ["drainage","highway-border","highway-fill","popplaces","provinces","provlines","road-border","road-fill","smallroads","raster-style"]

    it "can get layers" $ do
      m <- Map.create 512 512
      loadFixture m
      layers <- Map.getLayers m
      length layers `shouldBe` 5


    it "can get srs" $ do
      m <- Map.create 512 512
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

    it "can resize" $ do
      m <- Map.create 10 10
      img <- Map.render m 1
      BS.length (snd (toRgba8 img)) `shouldBe` (10*10*4)
      Map.resize m 300 200
      img2 <- Map.render m 1
      BS.length (snd (toRgba8 img2)) `shouldBe` (300*200*4)

    it "throws on invalid size" $ do
      m <- Map.create (-1) (-1)
      Map.render m 1 `shouldThrow` cppStdException

  describe "Projection" $ do
    it "can create valid" $
      Projection.parse merc `shouldSatisfy` isRight

    it "can toText valid" $
      fmap Projection.toText (Projection.parse merc) `shouldBe` Right "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs +over"


    it "cannot create invalid" $
      Projection.parse "foo" `shouldSatisfy` isLeft

    it "can trasform" $ do
      let Right src = Projection.parse merc
          Right dst = Projection.parse "+proj=lcc +ellps=GRS80 +lat_0=49 +lon_0=-95 +lat+1=49 +lat_2=77 +datum=NAD83 +units=m +no_defs"
          trans = projTransform src dst
          expected = Box { minx = 1372637.1001942465
                         , miny = -247003.8133187965
                         , maxx = 1746737.6177269476
                         , maxy = -25098.59307479199
                         }
      forward trans box `shouldBe` expected

  describe "Color" $ do
    it "can set good" $ do
      m <- Map.create 10 10
      let bg = RGBA 3 4 5 255
      Map.setBackground m bg
      Just bg2 <- Map.getBackground m
      bg2 `shouldBe` bg


  describe "Layer" $ do
    it "getDatasource returns Nothing if no datasource" $ do
      l <- Layer.create "fooo"
      ds <- Layer.getDatasource l
      ds `shouldSatisfy` isNothing

    it "getDatasource returns Just if datasource" $ do
      l <- Layer.create "fooo"
      Layer.setDatasource l =<< Datasource.create
        [ "type"     .= ("shape" :: Text)
        , "encoding" .= ("latin1" :: Text)
        , "file"     .= ("spec/data/popplaces" :: FilePath)
        ]
      ds <- Layer.getDatasource l
      ds `shouldSatisfy` isJust

    it "can get styles" $ do
      l <- Layer.create "fooo"
      sts <- Layer.getStyles l
      sts `shouldBe` []
      Layer.addStyle l "foo"
      Layer.addStyle l "bar"
      sts2 <- Layer.getStyles l
      sts2 `shouldBe` ["foo", "bar"]

  describe "Style" $ do
    it "can get rules" $ do
      m <- Map.create 512 512
      loadFixture m
      Just style <- lookup "provinces" <$> Map.getStyles m
      rs <- Style.getRules style
      length rs `shouldBe` 2

  describe "Rule" $ do
    it "no filter returns Nothing" $ do
      r <- Rule.create
      f <- Rule.getFilter r
      f `shouldBe` Nothing

    it "can get/set filter" $ do
      r <- Rule.create
      let Right e = Expression.parse expr
          expr = "([foo]='bar')"
      Rule.setFilter r e
      Just f <- Rule.getFilter r
      Expression.toText f `shouldBe` expr

    it "can get/set name" $ do
      r <- Rule.create
      let name = "foo"
      Rule.setName r name
      name' <- Rule.getName r
      name' `shouldBe` name

    it "can get symbolizers" $ do
      m <- Map.create 512 512
      loadFixture m
      Just style <- lookup "provinces" <$> Map.getStyles m
      [r1,r2] <- Style.getRules style
      syms1 <- Rule.getSymbolizers r1
      length syms1 `shouldBe` 1
      [sym] <- Rule.getSymbolizers r2
      Just f <- Rule.getFilter r2
      Expression.toText f `shouldBe` "([NOM_FR]='Québec')"
      mSym <- Symbolizer.unCreate sym
      let expected = Mapnik.polygon
                       & L.fill   ?~ Val (RGBA 217 235 203 255)
                       & L.compOp ?~ Val SrcOver
      mSym `shouldBe` expected

  describe "Parameters" $ do
    it "toList/fromList = id" $ do
      let params =
            [ "type"     .= ("shape" :: String)
            , "encoding" .= ("latin1" :: String)
            , "aDouble"  .= (7.4 :: Double)
            , "anInt"    .= (-45 :: Int)
            , "aTrue"    .= True
            , "aFalse"   .= False
            , "aNull"    .= (Nothing :: Maybe Int)
            , "aNullInt" .= (Just 5 :: Maybe Int)
            , "file"     .= ("spec/data/popplaces" :: String)
            ]
      Datasource.toList (Datasource.fromList params) `shouldMatchList` params

  describe "Datasource" $ do
    it "throws on invalid datasource" $
      Datasource.create ["type".= ("shapes" :: String)] `shouldThrow` cppStdException

  describe "Image" $ do
    it "can convert to rgba8 data and read it back" $ do
      m <- Map.create 10 10
      img <- Map.render m 1
      let rgba8 = Image.toRgba8 img
          Just img2  = Image.fromRgba8 rgba8
      BS.length (snd rgba8)  `shouldBe` (10*10*4)
      --BS.writeFile "map.webp" (Image.serialize "webp" img2)
      Image.serialize "png8" img `shouldBe` Image.serialize "png8" img2

    it "cannot create empty image" $
      Image.fromRgba8 ((0,0), "") `shouldSatisfy` isNothing

    it "doesnt serialize bad format" $ do
      let Just img = Image.fromRgba8 ((10, 10), (BS.replicate (4*10*10) 0))
      Image.serialize "bad" img `shouldSatisfy` isNothing

  describe "Expression" $ do
    it "can parse good expression" $ do
      let Right e = Expression.parse expr
          expr = "([foo]='bar')"
      Expression.toText e `shouldBe` expr

    it "cannot parse bad expression" $ do
      let e = Expression.parse "([foo"
      e `shouldSatisfy` isLeft

  describe "Transform" $ do
    it "can parse good transform" $ do
      let Right e = Transform.parse expr
          expr = "translate(2.1, -2.7)"
      Transform.toText e `shouldBe` expr

    it "cannot parse bad transform" $ do
      let e = Transform.parse "([foo"
      e `shouldSatisfy` isLeft

  describe "fromMapnik" $ do
    it "works for Map" $ do
      m' <- Map.create 512 512
      loadFixture m'
      m <- fromMapnik m'
      do
        let lns :: Traversal' Mapnik.Map DashArray
            lns = L.styles . at "provlines" . _Just
                . L.rules . ix 0
                . L.symbolizers . ix 0
                . L.strokeDashArray
                . _Just . L._Val
        m^?lns `shouldBe` Just [Dash 8 4, Dash 2 2, Dash 2 2]
      do
        let lns :: Traversal' Mapnik.Map Double
            lns = L.styles . at "raster-style" . _Just
                . L.rules . ix 0
                . L.symbolizers . ix 0
                . L.colorizer
                . _Just
                . L.stops
                . traverse
                . L.value
        m^..lns `shouldBe` [0,10..90]

  describe "TextPlacements" $ do
    it "can create" $ do
      let ps = Mapnik.def
      ps' <- TextPlacements.unCreate =<< TextPlacements.create ps
      ps `shouldBe` ps'

loadFixture :: Map -> IO ()
loadFixture m = do
  loadFixtureFrom "spec/map.xml" m
  Map.setSrs m merc
  Map.zoomToBox m box

loadFixtureFrom :: String -> Map -> IO ()
loadFixtureFrom p m = Map.loadXml m =<< BS.readFile p

box :: Box
box = Box (-8024477.28459) 5445190.38849 (-7381388.20071) 5662941.44855

merc :: Text
merc = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs +over"

cppStdException :: Selector CppException
cppStdException (CppStdException _) = True
cppStdException _ = False
