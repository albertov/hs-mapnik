module Main (main) where

import           Mapnik.Purescript (generatePS)
import           System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [outputDir] -> generatePS outputDir
    _ -> putStrLn "Usage: hs-mapnik-purs <outputDir>"
