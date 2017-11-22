{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module MapnikSpec (main, spec) where

import Mapnik
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
    describe "Map"   $ jsonProp (Proxy @Map)


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

