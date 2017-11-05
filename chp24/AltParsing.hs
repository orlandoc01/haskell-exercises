{-# LANGUAGE QuasiQuotes #-}
module AltParsing where
import Control.Applicative
import Data.Ratio ((%))
import Text.RawString.QQ
import Text.Trifecta

-- Number or String Applicative
type NumberOrString = Either Integer String
a = "blah"
b = "123"
c = "123blah79"

parseNos :: Parser NumberOrString
parseNos = 
  skipMany (oneOf "\n") *> 
  (Left <$> integer) <|> (Right <$> some letter) <*
  skipMany (oneOf "\n")

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

main1 = do
  print $ parseString (some parseNos) mempty eitherOr
  print $ parseString (some letter) mempty a
  print $ parseString integer mempty b
  print $ parseString parseNos mempty a
  print $ parseString parseNos mempty b
  print $ parseString (many parseNos) mempty c
  print $ parseString (some parseNos) mempty c
  print $ parseString parseNos mempty eitherOr

-- Fraction or Integers
type FractOrInt = Either Rational Integer

parseFraction :: Parser Rational
parseFraction = (%) <$> (decimal <* char '/') <*> decimal

parseFractInt :: Parser FractOrInt
parseFractInt =
      (Left <$> try parseFraction)
  <|> (Right <$> decimal)

