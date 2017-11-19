{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Mapnik.BindingsSpec (main, spec) where

import qualified Mapnik
import           Mapnik.Bindings
import qualified Mapnik.Lens as L
import qualified Mapnik.Bindings.Map as Map
import qualified Mapnik.Bindings.Layer as Layer
import qualified Mapnik.Bindings.Rule as Rule
import qualified Mapnik.Bindings.Style as Style
import qualified Mapnik.Bindings.Transform as Transform
import qualified Mapnik.Bindings.Expression as Expression
import qualified Mapnik.Bindings.Datasource as Datasource
import qualified Mapnik.Bindings.Symbolizer as Symbolizer
import qualified Mapnik.Bindings.TextPlacements as TextPlacements
import           Mapnik.QuickCheck

import           Control.Lens hiding ((.=))
import           Control.Monad
import           Data.Text (Text)
import           Data.Int
import           Data.IORef
import           Data.Maybe
import           Data.List (lookup)
import           Data.Either
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Generic as G
import qualified Data.ByteString as BS
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.IO ()


main :: IO ()
main = hspec spec

isPng :: BS.ByteString -> Bool
isPng s = BS.take 6 s == "\137PNG\r\n"

rSettings :: RenderSettings
rSettings = renderSettings 256 256 aBox

spec :: Spec
spec = beforeAll_ registerDefaults $ parallel $ do --replicateM_ 100 $ do

  describe "Map" $ do
    it "renders as PNG" $ do
      m <- Map.create
      loadFixture m
      img <- render m rSettings
      -- BS.writeFile "map.webp" (serialize "webp" img)
      let Just bs = serialize "png8" img
      bs `shouldSatisfy` isPng


    it "throws on broken XML" $ do
      m <- Map.create
      loadFixtureFrom "spec/bad.xml" m `shouldThrow` cppStdException

    it "can add layer and render" $ do
      m <- Map.create
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
      void $ render m rSettings

    it "can get styles" $ do
      m <- Map.create
      loadFixture m
      sts <- map fst <$> Map.getStyles m
      sts `shouldMatchList` ["drainage","highway-border","highway-fill","popplaces","provinces","provlines","road-border","road-fill","smallroads","raster-style"]

    it "can get layers" $ do
      m <- Map.create
      loadFixture m
      ls <- Map.getLayers m
      length ls `shouldBe` 5


    it "can get srs" $ do
      m <- Map.create
      Map.setSrs m merc
      s <- Map.getSrs m
      s `shouldBe` merc

    it "can get max extent when Nothing" $ do
      m <- Map.create
      e <- Map.getMaxExtent m
      e `shouldBe` Nothing

    it "can get max extent when Just" $ do
      m <- Map.create
      Map.setMaxExtent m aBox
      e <- Map.getMaxExtent m
      e `shouldBe` Just aBox

    it "throws on invalid size" $ do
      m <- Map.create
      render m (renderSettings (-1) (-1) aBox) `shouldThrow` cppStdException

  describe "Projection" $ do
    it "can trasform" $ do
      let dst = "+proj=lcc +ellps=GRS80 +lat_0=49 +lon_0=-95 +lat+1=49 +lat_2=77 +datum=NAD83 +units=m +no_defs"
          Right trans = projTransform merc dst
          expected = Box { minx = 1372637.1001942465
                         , miny = -247003.8133187965
                         , maxx = 1746737.6177269476
                         , maxy = -25098.59307479199
                         }
      forward trans aBox `shouldBe` expected

    it "can trasform with num points" $ do
      let dst = "+proj=lcc +ellps=GRS80 +lat_0=49 +lon_0=-95 +lat+1=49 +lat_2=77 +datum=NAD83 +units=m +no_defs"
          Right trans = projTransform merc dst
          expected = Box { minx = 1373921.9835390863
                         , miny = -247003.81331879605
                         , maxx = 1746737.6177269486
                         , maxy = -25098.593074791526
                         }
      fst (forward trans (aBox,100::Int)) `shouldBe` expected

    it "cannot create invalid" $
      projTransform merc "foo" `shouldSatisfy` isLeft

  describe "Color" $ do
    it "can set good" $ do
      m <- Map.create
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
      m <- Map.create
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
      let theName = "foo"
      Rule.setName r theName
      name' <- Rule.getName r
      name' `shouldBe` theName

    it "can get symbolizers" $ do
      m <- Map.create
      loadFixture m
      Just style <- lookup "provinces" <$> Map.getStyles m
      [r1,r2] <- Style.getRules style
      syms1 <- Rule.getSymbolizers r1
      length syms1 `shouldBe` 1
      [sym] <- Rule.getSymbolizers r2
      Just f <- Rule.getFilter r2
      Expression.toText f `shouldBe` "([NOM_FR]='Québec')"
      mSym <- Symbolizer.unCreate sym
      let expected = polygonSym
                       & L.fill   ?~ Val (RGBA 217 235 203 255)
                       & L.compOp ?~ Val SrcOver
      mSym `shouldBe` expected

  describe "Parameters" $ do
    it "paramsToList/paramsFromList = id" $ do
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
      Datasource.paramsToList (Datasource.paramsFromList params) `shouldMatchList` params

  describe "Datasource" $ do
    it "throws on invalid datasource" $
      Datasource.create ["type".= ("shapes" :: String)] `shouldThrow` cppStdException

    it "can get features" $ do
      ds <- Datasource.create
            [ "type"     .= ("shape" :: String)
            , "encoding" .= ("latin1" :: String)
            , "file"     .= ("spec/data/popplaces" :: String)
            ]
      let theBox = Box { minx = 1372637.1001942465
                       , miny = -247003.8133187965
                       , maxx = 1746737.6177269476
                       , maxy = -25098.59307479199
                       }
      let props = ["GEONAME", "SCALE_CAT"]
      (fs,feats) <- Datasource.features ds (queryBoxProps theBox props)
      --print feats
      length feats `shouldBe` 192
      G.toList fs `shouldMatchList` props
      G.toList (fields (head feats)) `shouldMatchList` [TextValue "Sorel-Tracy", IntValue 0]
      toWkt (fromJust (geometry (head feats))) `shouldBe` "POINT(1681422.74999858 -39049.2656230889)"

  describe "Image" $ do
    it "can convert to rgba8 data and read it back" $ do
      m <- Map.create
      img <- render m rSettings
      let rgba8 = toRgba8 img
          Just img2  = fromRgba8 rgba8
          RenderSettings{width=w, height=h} = rSettings
      G.length (snd rgba8)  `shouldBe` (w * h)
      --BS.writeFile "map.webp" (serialize "webp" img2)
      serialize "png8" img `shouldBe` serialize "png8" img2

    it "cannot create empty image" $
      fromRgba8 ((0,0), G.empty) `shouldSatisfy` isNothing

    it "doesnt serialize bad format" $ do
      let Just img = fromRgba8 ((10, 10), (G.replicate (10*10) (PixelRgba8 0)))
      serialize "bad" img `shouldSatisfy` isNothing

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
      m' <- Map.create
      loadFixture m'
      m <- fromMapnik m'
      do
        let lns :: Traversal' Map DashArray
            lns = L.styles . at "provlines" . _Just
                . L.rules . ix 0
                . L.symbolizers . ix 0
                . L.strokeDashArray
                . _Just . L._Val
        m^?lns `shouldBe` Just [Dash 8 4, Dash 2 2, Dash 2 2]
      do
        let lns :: Traversal' Map Double
            lns = L.styles . at "raster-style" . _Just
                . L.rules . ix 0
                . L.symbolizers . ix 0
                . L.colorizer
                . _Just
                . L.stops
                . traverse
                . L.value
        m^..lns `shouldBe` [0,100,200,400,800,1600,3200,6400,12800,25600]

  describe "TextPlacements" $ do
    it "can create" $ do
      let ps = def
      ps' <- TextPlacements.unCreate =<< TextPlacements.create ps
      ps `shouldBe` ps'

  describe "HsVector" $ do
    it "can create and render" $ do
      m <- Map.create
      loadFixture m
      Map.removeAllLayers m
      Map.setSrs m "+init=epsg:3857"
      let theExtent = Box 0 0 100 100

      imgBase <- render m (renderSettings 512 512 theExtent)
      --BS.writeFile "map.webp" (fromJust (serialize "webp" imgBase))
      snd (toRgba8 imgBase) `shouldSatisfy` G.all (== transparent)

      l <- Layer.create "fooo"
      Layer.setSrs l "+init=epsg:3857"
      ref <- newIORef Nothing
      ds <- Datasource.createHsDatasource HsVector
        { name = "fooo"
        , extent = theExtent
        , fieldNames = ["aString", "anInt", "aBool", "aDouble", "aNull"]
        , getFeatures = \q -> do
            writeIORef ref (Just q)
            return [ feature
                     { fid=105
                     , geometry=Just "LINESTRING (30 10, 10 30, 40 40)"
                     , fields=["batróñ~", IntValue 3, BoolValue False, DoubleValue 3.2, NullValue]
                     }
                   ]
        , getFeaturesAtPoint = \_ _ -> return []
        }
      (_,feats) <- Datasource.features ds (queryBox theExtent)
      raster (head feats) `shouldSatisfy` isNothing
      Layer.setDatasource l ds
      Layer.addStyle l "provlines"
      Map.addLayer m l
      img <- render m (renderSettings 512 512 theExtent)
      snd (toRgba8 img) `shouldSatisfy` G.any (/= transparent)
      Just q <- readIORef ref
      box q `shouldBe` theExtent
      unBufferedBox q `shouldBe` theExtent
      resolution q `shouldBe` Pair 5.12 5.12

  describe "HsRaster" $ do
    it "can create and render" $ do
      m <- Map.create
      loadFixture m
      Map.removeAllLayers m
      Map.setSrs m "+init=epsg:3857"
      l <- Layer.create "fooo"
      Layer.setSrs l "+init=epsg:3857"
      let theExtent = Box 0 0 100 100
          boxes = [Box 0 0 50 50, Box 50 50 100 100]
          thePixels = G.generate (50*50) fromIntegral :: St.Vector Int32
      ds <- Datasource.createHsDatasource HsRaster
        { name = "fooo"
        , extent = theExtent
        , fieldNames = []
        , getRasters = \_ ->
            return $ flip map boxes $ \b ->
              Raster
                { extent = b
                , queryExtent = b
                , filterFactor = 1
                , width = 50
                , height = 50
                , nodata = Nothing
                , pixels = thePixels
                }
        , getFeaturesAtPoint = \_ _ -> return []
        }
      (_,feats) <- Datasource.features ds (queryBox theExtent)
      geometry (head feats) `shouldSatisfy` isNothing
      let Just r = raster (head feats)
      getPixels r `shouldBe` Just thePixels
      Layer.setDatasource l ds
      Layer.addStyle l "raster-style"
      Map.addLayer m l
      let h=256; w=256
      img <- render m (rSettings { extent = theExtent, width=w, height=h })
      --BS.writeFile "map.webp" (fromJust (serialize "webp" img))
      let (_, imData) = toRgba8 img
      imData G.! 0 `shouldBe` transparent
      imData G.! 128 `shouldNotBe` transparent
      imData G.! (256*128) `shouldNotBe` transparent
      imData G.! (256*128+128) `shouldBe` transparent

  it "can pass render variables" $ do
    m <- Map.create
    loadFixture m
    Map.removeAllLayers m
    l <- Layer.create "fooo"
    ref <- newIORef Nothing
    let theExtent = Box 0 0 100 100
    ds <- Datasource.createHsDatasource HsVector
      { name = "fooo"
      , extent = theExtent
      , fieldNames = []
      , getFeatures = \q -> do
          writeIORef ref (Just q)
          --print q
          return []
      , getFeaturesAtPoint = \_ _ -> return []
      }
    Layer.setDatasource l ds
    Layer.addStyle l "provlines"
    Map.addLayer m l
    let vars = [ ("foo", DoubleValue 2.4)
               , ("bar", IntValue 42)
               , ("bar", NullValue)
               , ("mar", BoolValue False)
               , ("car", TextValue "some text wíẗḧ unicode")
               ]
    _ <- render m (renderSettings 512 512 theExtent) { variables = vars }
    Just q <- readIORef ref
    Datasource.variables q `shouldBe` vars


  describe "property tests" $ do
    prop "fromMapnik <=< toMapnik = return" $ \(setExistingDatasources -> a) -> do
      -- We can't really compare equality here because we need to account for
      -- the default values. Instead of re-implementing mapnik's logic we
      -- check that the xml serialization done by mapnik (which normalizes
      -- default values) of an arbitrary map `a` is the same as
      -- `(fromMapnik <=< toMapnik) a`
      a' <- (fromMapnik <=< toMapnik) a
      mapXmlEq a a' `shouldReturn` True
  
    prop "render preserves Map observable configuration" $ \opts -> do
      m <- fromFixture
      xml1 <- Map.toXml m
      _ <- render m opts
      xml2 <- Map.toXml m
      xml1 `shouldBe` xml2
      

mapXmlEq :: ( MapnikType a2 ~ Map.Map
            , MapnikType a1 ~ Map.Map
            , ToMapnik a1, ToMapnik a2
            ) => a1 -> a2 -> IO Bool
mapXmlEq a b =
  (==) <$> (Map.toXml =<< toMapnik a)
       <*> (Map.toXml =<< toMapnik b)

-- we need to make sure the datasource exists or loadMap will barf
setExistingDatasources :: Mapnik.Map -> Mapnik.Map
setExistingDatasources = L.layers . traverse . L.dataSource ?~ existingDatasource
  where
    existingDatasource = Datasource
      [ "type"     .= ("shape" :: String)
      , "encoding" .= ("latin1" :: String)
      , "file"     .= ("spec/data/popplaces" :: String)
      ]

loadFixture :: Map.Map -> IO ()
loadFixture = loadFixtureFrom "spec/map.xml"

fromFixture :: IO Map.Map
fromFixture = Map.fromXml =<< BS.readFile "spec/map.xml"

loadFixtureFrom :: FilePath -> Map.Map -> IO ()
loadFixtureFrom p m = Map.loadXml m =<< BS.readFile p

transparent :: PixelRgba8
transparent = PixelRgba8 0xFFFFFFFF

aBox :: Box
aBox = Box (-8024477.28459) 5445190.38849 (-7381388.20071) 5662941.44855

merc :: Text
merc = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs +over"

cppStdException :: Selector CppException
cppStdException (CppStdException _) = True
cppStdException _ = False

instance Arbitrary RenderSettings where
  arbitrary = do
    width <- getPositive <$> arbitrary
    height <- getPositive <$> arbitrary
    extent <- arbitrary
    variables <- arbitrary
    scaleFactor <- getPositive <$> arbitrary
    srs <- maybeArb arbitrarySrs
    aspectFixMode <- arbitrary
    return RenderSettings{..}
