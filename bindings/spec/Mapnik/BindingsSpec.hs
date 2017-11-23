{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Mapnik.BindingsSpec (main, spec) where

import           Mapnik.Bindings
import qualified Mapnik.Bindings.Map as Map
import qualified Mapnik.Bindings.Layer as Layer
import qualified Mapnik.Bindings.Transform as Transform
import qualified Mapnik.Bindings.Expression as Expression
import qualified Mapnik.Bindings.Datasource as Datasource
import           Mapnik.QuickCheck

import           Control.Lens hiding ((.=))
import           Control.Exception
import           Control.Monad
import           Data.Text (Text)
import           Data.Typeable
import           Data.Int
import           Data.IORef
import           Data.Maybe
import           Data.Either
import qualified Data.Vector.Generic as G
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.IO ()
import           Prelude hiding (filter)
import           System.Directory (listDirectory)
import           System.FilePath ((</>))


main :: IO ()
main = hspec spec

isPng :: BS.ByteString -> Bool
isPng s = BS.take 6 s == "\137PNG\r\n"

rSettings :: RenderSettings
rSettings = renderSettings 256 256 aBox

spec :: Spec
spec = beforeAll_ registerDefaults $ parallel $ do --replicateM_ 500 $ do

  describe "Map" $ do
    it "renders as PNG" $ do
      m <- fromFixture
      img <- render m rSettings
      -- BS.writeFile "map.webp" (serialize "webp" img)
      fmap isPng (serialize "png8" img) `shouldBe` Just True

    it "throws on broken XML" $ do
      Map.fromXmlFile ("spec"</>"bad.xml") `shouldThrow` cppStdException

    it "fromMapnik map.xml returns expected values" $ do
      m <- fromMapnik =<< fromFixture

      m^.layers.to length  `shouldBe` 5

      m^.srs `shouldBe` Just merc

      m^? styles.at "provlines"._Just.rules.ix 0.symbolizers.ix 0
          .strokeDashArray._Just._Val
        `shouldBe` Just [Dash 8 4, Dash 2 2, Dash 2 2]

      m^..styles.at "raster-style"._Just.rules
           .ix 0.symbolizers
           .ix 0.colorizer._Just.stops.traverse.value
        `shouldBe` [0,100,200,400,800,1600,3200,6400,12800,25600]

      m^.styles.to M.keys  `shouldMatchList` [ "drainage"
                                             , "highway-border"
                                             , "highway-fill"
                                             , "popplaces"
                                             , "provinces"
                                             , "provlines"
                                             , "road-border"
                                             , "road-fill"
                                             , "smallroads"
                                             , "raster-style" ]

      m^?styles.at "provinces"._Just.rules.to length `shouldBe` Just 2

      m^?styles.at "provinces"._Just.rules.ix 0.symbolizers.to length
        `shouldBe` Just 1

      m^?styles.at "provinces"._Just.rules.ix 1.filter._Just
        `shouldBe` Just "([NOM_FR]='Québec')"

      m^?styles.at "provinces"._Just.rules.ix 1.symbolizers.ix 0
        `shouldBe` Just ( polygonSym
                        & fill   ?~ Val (RGBA 217 235 203 255)
                        & compOp ?~ Val SrcOver)


    it "can add layer and render" $ do
      m <- fromFixture
      Map.removeAllLayers m
      l <- Layer.create "Populated places"
      Layer.setSrs l "+proj=lcc +ellps=GRS80 +lat_0=49 +lon_0=-95 +lat+1=49 +lat_2=77 +datum=NAD83 +units=m +no_defs"
      Layer.addStyle l "popplaces"
      Layer.setDatasource l =<< Datasource.create
        [ "type"     .= ("shape" :: String)
        , "encoding" .= ("latin1" :: String)
        , "file"     .= ("spec"</>"data"</>"popplaces")
        ]
      Map.addLayer m l
      void $ render m rSettings


    it "throws on invalid size" $ do
      m <- Map.create
      render m (renderSettings (-1) (-1) aBox) `shouldThrow` cppStdException

    it "can parse and serialize all mapnik test maps" $ do
      let styleDir = "spec"</>"data"</>"visual"</>"styles"
      maps <- listDirectory styleDir
      forM_ maps $ \sty -> do
        m1 <- Map.fromXmlFile (styleDir</>sty)
        xml1 <- Map.toXml m1
        m2 <- (toMapnik <=< fromMapnik) m1
        Map.toXml m2 `shouldReturn` xml1

  describe "Projection" $ do
    it "can trasform" $ do
      let dst = "+proj=lcc +ellps=GRS80 +lat_0=49 +lon_0=-95 +lat+1=49 +lat_2=77 +datum=NAD83 +units=m +no_defs"
          Right trans = projTransform merc dst
          expected = Box 1372637.1001942465
                         (-247003.8133187965)
                         1746737.6177269476
                         (-25098.59307479199)
      forward trans aBox `shouldBe` expected --TODO: compare loosely for portability

    it "can trasform with num points" $ do
      let dst = "+proj=lcc +ellps=GRS80 +lat_0=49 +lon_0=-95 +lat+1=49 +lat_2=77 +datum=NAD83 +units=m +no_defs"
          Right trans = projTransform merc dst
          expected = Box 1373921.9835390863
                         (-247003.81331879605)
                         (1746737.6177269486)
                         (-25098.593074791526)
      fst (forward trans (aBox,100::Int)) `shouldBe` expected --TODO: compare loosely for portability

    it "cannot create invalid" $
      projTransform merc "foo" `shouldSatisfy` isLeft


  describe "Datasource" $ do
    it "throws on invalid datasource" $
      Datasource.create ["type".= ("shapes" :: String)] `shouldThrow` cppStdException

    it "can get features" $ do
      ds <- Datasource.create
            [ "type"     .= ("shape" :: String)
            , "encoding" .= ("latin1" :: String)
            , "file"     .= ("spec"</>"data"</>"popplaces")
            ]
      let theBox = Box 1372637.1001942465
                       (-247003.8133187965)
                       1746737.6177269476
                       (-25098.59307479199)
          props = ["GEONAME", "SCALE_CAT"]
      (fs,feats) <- Datasource.features ds (queryBoxProps theBox props)
      --print feats
      length feats `shouldBe` 192
      G.toList fs `shouldMatchList` props
      G.toList (fields (head feats)) `shouldMatchList` [TextValue "Sorel-Tracy", IntValue 0]
      toWkt (fromJust (geometry (head feats))) `shouldBe` "POINT(1681422.74999858 -39049.2656230889)"

    describe "HsDataSource" $ do
      it "can create and render HsVector" $ do
        m <- fromFixture
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
          { _extent = theExtent
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
        q^.box `shouldBe` theExtent
        q^.unBufferedBox `shouldBe` theExtent
        q^.resolution `shouldBe` Pair 5.12 5.12

      it "can create and render HsRaster" $ do
        m <- fromFixture
        Map.removeAllLayers m
        Map.setSrs m "+init=epsg:3857"
        l <- Layer.create "fooo"
        Layer.setSrs l "+init=epsg:3857"
        let theExtent = Box 0 0 100 100
            boxes = [Box 0 0 50 50, Box 50 50 100 100]
            thePixels = G.generate (50*50) fromIntegral
        ds <- Datasource.createHsDatasource HsRaster
          { _extent = theExtent
          , fieldNames = []
          , getRasters = \_ ->
              return $ flip map boxes $ \b ->
                mkRaster b (50,50) thePixels :: Raster Int32
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
        img <- render m $ rSettings
                        & extent .~ theExtent
                        & width  .~ w
                        & height .~ h
        --BS.writeFile "map.webp" (fromJust (serialize "webp" img))
        let (_, imData) = toRgba8 img
        imData G.! 0 `shouldBe` transparent
        imData G.! 128 `shouldNotBe` transparent
        imData G.! (256*128) `shouldNotBe` transparent
        imData G.! (256*128+128) `shouldBe` transparent

      it "can throw exception" $ do
        let theExtent = Box 0 0 100 100
        ds <- Datasource.createHsDatasource HsRaster
          { _extent = theExtent
          , fieldNames = []
          , getRasters = \_ ->
              throwIO TestException :: IO [Raster Int32]
          , getFeaturesAtPoint = \_ _ -> return []
          }
        Datasource.features ds (queryBox theExtent) `shouldThrow` testException

      it "can receive render variables" $ do
        m <- fromFixture
        Map.removeAllLayers m
        l <- Layer.create "fooo"
        ref <- newIORef Nothing
        let theExtent = Box 0 0 100 100
        ds <- Datasource.createHsDatasource HsVector
          { _extent = theExtent
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
        _ <- render m $ renderSettings 512 512 theExtent & variables .~  vars
        Just q <- readIORef ref
        q^.variables `shouldBe` vars

  describe "Image" $ do
    it "can convert to rgba8 and read it back" $ do
      m <- Map.create
      img <- render m rSettings
      let rgba8 = toRgba8 img
          Just img2  = fromRgba8 rgba8
          w = rSettings^?!width
          h = rSettings^?!height
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


  describe "property tests" $ do
    describe "Map" $ parallel $ do
      let cleanupMap
            = setExistingDatasources
            -- ^ we need to make sure the datasource exists or loadMap will barf
            . setExistingFontDir
            -- ^ we need to make sure the font dir exists or toMapnik(Map) will barf
            --
      prop "fromMapnik <=< toMapnik = return" $ \(cleanupMap -> a) -> do
        -- We can't really compare equality here because we need to account for
        -- the default values. Instead of re-implementing mapnik's logic we
        -- check that the xml serialization done by mapnik (which normalizes
        -- default values) of an arbitrary map `a` is the same as
        -- `(fromMapnik <=< toMapnik) a`
        a' <- (fromMapnik <=< toMapnik) a
        mapXmlEq a a' `shouldReturn` True

      prop "can render or not (but dont segfault)" $ \(cleanupMap -> a, evil, opts) -> do
        let dirtyMap = if not evil then id
                        else fontSets .~ mempty
        swallowExceptions $
          flip render opts =<< toMapnik (dirtyMap a)

      prop "render preserves Map observable configuration" $ \opts -> do
        m <- fromFixture
        xml1 <- Map.toXml m
        _ <- render m opts
        xml2 <- Map.toXml m
        xml1 `shouldBe` xml2

    describe "Parameters" $ do
      prop "paramsToList.paramsFromList = id" $ \params ->
        (Datasource.paramsToMap . Datasource.paramsFromMap) params
          `shouldBe` params

swallowExceptions :: IO a -> IO ()
swallowExceptions = void . try @SomeException

mapXmlEq :: ( MapnikType a2 ~ Map.Map
            , MapnikType a1 ~ Map.Map
            , ToMapnik a1, ToMapnik a2
            ) => a1 -> a2 -> IO Bool
mapXmlEq a b =
  (==) <$> (Map.toXml =<< toMapnik a)
       <*> (Map.toXml =<< toMapnik b)

setExistingDatasources :: Map -> Map
setExistingDatasources = layers . traverse . dataSource ?~ existingDatasource
  where
    existingDatasource = Datasource
      [ "type"     .= ("shape" :: String)
      , "encoding" .= ("latin1" :: String)
      , "file"     .= ("spec"</>"data"</>"popplaces")
      ]

setExistingFontDir :: Map -> Map
setExistingFontDir =
  fontDirectory ?~ "spec"</>"data"</>"visual"</>"fonts"</>"Awesome"

fromFixture :: IO Map.Map
fromFixture = Map.fromXmlFile ("spec"</>"map.xml")

transparent :: PixelRgba8
transparent = PixelRgba8 0xFFFFFFFF

aBox :: Box
aBox = Box (-8024477.28459) 5445190.38849 (-7381388.20071) 5662941.44855

merc :: Text
merc = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs +over"

cppStdException :: Selector MapnikError
cppStdException (CppStdException _) = True
cppStdException _ = False

instance Arbitrary RenderSettings where
  arbitrary = do
    _renderSettingsWidth <- choose (1, 100)
    _renderSettingsHeight <- choose (1, 100)
    _renderSettingsExtent <- oneof [arbitrary, pure aBox]
    _renderSettingsVariables <- arbitrary
    _renderSettingsScaleFactor <- getPositive <$> arbitrary
    _renderSettingsSrs <- maybeArb arbitrarySrs
    _renderSettingsAspectFixMode <- arbitrary
    return RenderSettings{..}

data TestException = TestException
  deriving (Eq, Show, Typeable, Exception)

testException :: Selector TestException
testException TestException = True
