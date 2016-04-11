module MapnikSpec (main, spec) where

import qualified Mapnik

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "smoke test" $ do

    it "compiles" $ do
      True `shouldBe` True
