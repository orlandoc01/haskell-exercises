{-# LANGUAGE OverloadedStrings #-}
module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"
alsoBad :: IsString s => s
alsoBad = "10"
shouldWork :: IsString s => s
shouldWork = "1/2"
shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = pure (%) <*> (decimal <* char '/') <*> decimal
-- parseFraction = do
--   numerator <- decimal
--   char '/'
--   denominator <- decimal
--   return (numerator % denominator)

virtuousFraction :: (Monad m, TokenParsing m) => m Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseIntWithEof :: Parser Integer
parseIntWithEof = decimal <* eof

main :: IO ()
main = do
  print $ parseString virtuousFraction mempty shouldWork
  print $ parseString virtuousFraction mempty shouldAlsoWork
  print $ parseString virtuousFraction mempty badFraction
  print $ parseString virtuousFraction mempty alsoBad

main2 :: IO ()
main2 = do
  print $ parseOnly virtuousFraction badFraction
  print $ parseOnly virtuousFraction shouldWork
  print $ parseOnly virtuousFraction shouldAlsoWork
  print $ parseOnly virtuousFraction alsoBad

  print $ parseString virtuousFraction mempty badFraction
  print $ parseString virtuousFraction mempty shouldWork
  print $ parseString virtuousFraction mempty shouldAlsoWork
  print $ parseString virtuousFraction mempty alsoBad
