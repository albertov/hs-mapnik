{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.Color (
  Color(..)
, parse
, toText
, colorParser
) where

import Mapnik.Imports
import Mapnik.Util

import Data.Monoid
import Data.Word
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import Data.Attoparsec.Text hiding (parse)

data Color = RGBA !Word8 !Word8 !Word8 !Word8
  deriving (Eq, Show, Generic)

deriveMapnikJSON ''Color

parse :: Text -> Either String Color
parse = parseOnly (colorParser <* endOfInput)

colorParser :: Parser Color
--TODO Hex parser
colorParser = choice [ rgb, rgba ] where
  rgb = "rgb" *> bracketed (do
    [r,g,b] <- sepByCommas decimal
    pure (RGBA r g b 255)
    )
  rgba = "rgba" *> bracketed (do
    r <- decimal <* commaWs
    g <- decimal <* commaWs
    b <- decimal <* commaWs
    a <- double
    pure (RGBA r g b (round (a*255)))
    )


toText :: Color -> Text
toText (RGBA r g b 255) = toStrict $ toLazyText $
  "rgb(" <> B.decimal r <> ", "
         <> B.decimal g <> ", "
         <> B.decimal b
         <> ")"
toText (RGBA r g b a) = toStrict $ toLazyText $
  "rgba(" <> B.decimal r <> ", "
          <> B.decimal g <> ", "
          <> B.decimal b <> ", "
          <> B.formatRealFloat B.Fixed (Just 3) (fromIntegral a / 255 :: Float)
          <> ")"
