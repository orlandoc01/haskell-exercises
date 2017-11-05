{-# LANGUAGE QuasiQuotes #-}
{-# LANGAUGE OverloadedStrings #-}
module Data.Ini where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

headerEx :: ByteString
headerEx = "[blah]"

commentEx :: ByteString
commentEx = "; last modified 1 April 2001 by John Doe"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n  \n;hah"

assignmentEx :: ByteString
assignmentEx = "woot=1"

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intootheandclaw
|]


skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments = skipMany $ (char ';' <|> char '#') >> skipMany (noneOf "\n") >> skipEOL

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

newtype Header = Header String deriving (Eq, Ord, Show)
parseHeader :: Parser Header
parseHeader = parseBracketPair $ Header <$> some letter


type Name = String
type Value = String
type Assignments = Map Name Value
parseAssignment :: Parser (Name, Value)
parseAssignment = (,) <$> getName <*> getValue
    where getName = some letter <* char '='
          getValue = some (noneOf "\n") <* skipEOL


data Section = Section Header Assignments deriving (Eq, Show)
newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments);

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) = M.insert h a

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)

maybeSuccess :: Result a -> Maybe a
maybeSuccess x = case x of (Success a) -> Just a; _ -> Nothing

main :: IO ()
main = hspec $ do
  describe "Assignment Parsing" $
    it "can parse a simple assignment" $ do
      let m = parseByteString parseAssignment mempty assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")

  describe "Header Parsing" $
    it "can parse a simple header" $ do
      let m = parseByteString parseHeader mempty headerEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  describe "Commnet parsing" $
    it "can skip a comment before a header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
          m = parseByteString p mempty i
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  describe "Section Parasing" $
    it "can parse a simplesection" $ do
      let m = parseByteString parseSection mempty sectionEx
          r' = maybeSuccess m
          states = M.fromList[("Chris", "Texas")]
          expected' = Just (Section (Header "states") states) 
      print m
      r' `shouldBe` expected'

  describe "INI Parsing" $
    it "Can parse multiple secions" $ do
      let m = parseByteString parseIni mempty sectionEx''
          r' = maybeSuccess m
          sectionValues = M.fromList [("alias", "claw"), ("host", "wikipedia.org")]
          whatisitValues = M.fromList [("red", "intoothandclaw")]
          expected' = Just (Config (M.fromList [
                            (Header "section", sectionValues), 
                            (Header "whatisit", whatisitValues)]))
      print m
      r' `shouldBe` expected'
