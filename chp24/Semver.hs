{-# LANGUAGE OverloadedStrings #-}
module SemVer where

import Control.Applicative
import Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)


parseSemVer :: Parser SemVer
parseSemVer = SemVer <$> major <*> minor <*> patch <*> release <*> metadata
  where major = decimal <* char '.'
        minor = decimal <* char '.'
        patch = decimal
        release = try (char '-' *> dotSep numOrString) <|> pure []
        metadata = try (char '+' *> dotSep numOrString) <|> pure []
        numOrString = try (NOSI <$> decimal) <|> NOSS <$> some letter
        dotSep p = sepBy1 p dot


parseDigit :: Parser Char
parseDigit = digit

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)
  

-- parsePhoneNumber :: Parser PhoneNumber
-- parsePhoneNumber = PhoneNumber <$> get3 <*> get3 <*> get3
--   where get3 = :

getNums :: Parser String
getNums = some (skipMany (noneOf "1234567890") *> digit)



