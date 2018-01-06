{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module MapnikSpec (main, spec) where

import Mapnik
import Mapnik.ImageFilter as IF
import Mapnik.Symbolizer (parsePlacementPosition, placementPositionToText)
import Mapnik.Color as Color
import Mapnik.QuickCheck ()

import Data.Proxy
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "JSON" $ do
    describe "Map"   $ jsonProp (Proxy @(Map Datasource))

  describe "ImageFilter" $ do
    prop "parse (toText x) = Right x" $ \x ->
      let res = IF.parse str
          str = IF.toText x
      in counterexample (show (str, res)) $ res == Right x

    prop "parseMany (toTextMany x) = Right x" $ \x ->
      let res = IF.parseMany str
          str = IF.toTextMany x
      in counterexample (show (str, res)) $ res == Right x

  describe "Color" $
    prop "parse (toText x) = Right x" $ \x ->
      let res = Color.parse str
          str = Color.toText x
      in counterexample (show (str, res)) $ res == Right x

  describe "SimplePlacementPosition" $
    prop "parse (toText x) = Right x" $ \x ->
      let res = parsePlacementPosition str
          str = placementPositionToText (Val x)
      in counterexample (show (str, res)) $ res == Right (Val x)

jsonProp
  :: forall p a.
   ( Show a
   , Eq a
   , ToJSON a
   , FromJSON a
   , Arbitrary a
   )
  => p a
  -> SpecWith ()
jsonProp _ = parallel $
  prop "decode (encode a) = Just a" $ \(a :: a) ->
    decode (encode a) == Just a

