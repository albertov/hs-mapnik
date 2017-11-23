{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mapnik.ImageFilter (
  ImageFilter(..)
, ColorStop (..)
, parse
, parseMany
, toText
, toTextMany
, imageFilterParser
) where


import Mapnik.Imports
import Mapnik.Util
import Mapnik.Color (Color, colorParser)
import qualified Mapnik.Color as Color

import Data.Monoid
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text hiding (parse)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder.RealFloat as B


data ColorStop = ColorStop Color Double
  deriving (Eq, Show, Generic)
deriveMapnikJSON ''ColorStop

data ImageFilter
  = Blur
  | Emboss
  | Sharpen
  | EdgeDetect
  | Sobel
  | Gray
  | XGradient
  | YGradient
  | Invert
  | ColorBlindProtanope
  | ColorBlindDeuteranope
  | ColorBlindTritanope
  | AggStackBlur Int Int
  | ColorToAlpha Color
  | ScaleHsla Double Double Double Double Double Double
  | ColorizeAlpha [ColorStop]
  deriving (Eq, Show, Generic)
deriveMapnikJSON ''ImageFilter

parseMany :: Text -> Either String [ImageFilter]
parseMany = parseOnly (imageFilterParser `sepBy` space <* endOfInput)

parse :: Text -> Either String ImageFilter
parse = parseOnly (imageFilterParser <* endOfInput)

imageFilterParser :: Parser ImageFilter
imageFilterParser = choice [
    "blur" *> pure Blur
  , "emboss" *> pure Emboss
  , "sharpen" *> pure Sharpen
  , "edge-detect" *> pure EdgeDetect
  , "sobel" *> pure Sobel
  , "gray" *> pure Gray
  , "x-gradient" *> pure XGradient
  , "y-gradient" *> pure YGradient
  , "invert" *> pure Invert
  , "color-blind-protanope" *> pure ColorBlindProtanope
  , "color-blind-deuteranope" *> pure ColorBlindDeuteranope
  , "color-blind-tritanope" *> pure ColorBlindTritanope
  , "agg-stack-blur" *> bracketed (do
      [rx,ry] <- sepByCommas (signed decimal)
      pure (AggStackBlur rx ry)
      )
  , "scale-hsla" *> bracketed (do
      [a,b,c,d,e,f] <- sepByCommas (signed double)
      pure (ScaleHsla a b c d e f)
      )
  , "color-to-alpha" *> (ColorToAlpha <$> bracketed (stripWs colorParser))
  , "colorize-alpha" *> (ColorizeAlpha <$> bracketed (sepByCommas stopParser))
  ]
  where
    stopParser = ColorStop <$> colorParser <*> (skipSpace *> signed double)


toText :: ImageFilter -> Text
toText Blur = "blur"
toText Emboss = "emboss"
toText Sharpen = "sharpen"
toText EdgeDetect = "edge-detect"
toText Sobel = "sobel"
toText Gray = "gray"
toText XGradient = "x-gradient"
toText YGradient = "y-gradient"
toText Invert = "invert"
toText ColorBlindProtanope = "color-blind-protanope"
toText ColorBlindDeuteranope = "color-blind-deuteranope"
toText ColorBlindTritanope = "color-blind-tritanope"
toText (AggStackBlur a b) = toStrict $ toLazyText $
  "agg-stack-blur(" <> B.decimal a <> ", " <> B.decimal b <> ")"
toText (ColorToAlpha a) = toStrict $ toLazyText $
  "color-to-alpha(" <> B.fromText (Color.toText a) <> ")"
toText (ScaleHsla a b c d e f) = toStrict $ toLazyText $ (mconcat $
  "scale-hsla(" : intersperse ", " nums) <> ")"
  where nums = map (B.formatRealFloat B.Fixed Nothing) [a,b,c,d,e,f]
toText (ColorizeAlpha stops) = toStrict $ toLazyText $ (mconcat $
  "colorize-alpha(" : intersperse ", " (map strStop stops)) <> ")"
  where
    strStop (ColorStop c o) =
      B.fromText (Color.toText c) <> " " <> B.formatRealFloat B.Fixed Nothing o

toTextMany :: [ImageFilter] -> Text
toTextMany = T.intercalate " " . map toText
