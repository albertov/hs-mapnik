{-# LANGUAGE OverloadedStrings #-}
module Mapnik.Util (
  bracketed
, sepByCommas
, commaWs
, stripWs
, cppDouble
) where

import Data.Attoparsec.Text
import Data.Char (isDigit)
import Data.Monoid

bracketed :: Parser a -> Parser a
bracketed p = skipSpace *> char '(' *> skipSpace *> p <* skipSpace <* char ')'

sepByCommas :: Parser a -> Parser [a]
sepByCommas = (`sepBy'` commaWs)

commaWs :: Parser Char
commaWs = stripWs (char ',')

stripWs :: Parser a -> Parser a
stripWs p = skipSpace *> p <* skipSpace

cppDouble :: Parser Double
cppDouble = do
  c <- peekChar'
  if c /= '.' then double else do
    s <- takeWhile1 (\x -> isDigit x || x=='.')
    reparseWith double ("0"<>s)
  where reparseWith p = either (fail "invalid double") return . parseOnly p
