module MapnikSpec (main, spec) where

import Mapnik
import Mapnik.Map

import           Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do

  it "moves" $ do
    2 `shouldBe` 2
