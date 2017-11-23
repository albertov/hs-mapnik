module Mapnik.Util (
  bracketed
, sepByCommas
, commaWs
, stripWs
) where

import Data.Attoparsec.Text

bracketed :: Parser a -> Parser a
bracketed p = skipSpace *> char '(' *> skipSpace *> p <* skipSpace <* char ')'

sepByCommas :: Parser a -> Parser [a]
sepByCommas = (`sepBy'` commaWs)

commaWs :: Parser Char
commaWs = stripWs (char ',')

stripWs :: Parser a -> Parser a
stripWs p = skipSpace *> p <* skipSpace
