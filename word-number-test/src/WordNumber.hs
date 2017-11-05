module WordNumber where
import Data.Char (digitToInt)
import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits = map digitToInt . show 

wordNumber :: Int -> String
wordNumber i = concat $ intersperse "-" words
  where words = map digitToWord $ digits i
