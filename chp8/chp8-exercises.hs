module Chp8 where
import Data.Char (digitToInt)
import Data.List (intersperse)

sum' :: (Eq a, Num a) => a -> a
sum' 0 = 0
sum' n = n + sum' (n - 1)

multiply' :: (Integral a) => a -> a -> a
multiply' 0 _ = 0
multiply' _ 0 = 0
multiply' x y = y + multiply' (x - 1) y

data DividedResult a = Result (a,a) | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy num divisor = go num divisor 0 where 
  go n d count
    | d == 0 = DividedByZero
    | abs(n) < abs(d) = Result (count, n)
    | sameSigns n d = go (n - d) d (count + 1)
    | otherwise = go (n + d) d (count - 1)
      where sameSigns n d = signum n == signum d

mc91 :: Integral a => a -> a
mc91 x
  | x > 100 = x - 10
  | otherwise = 91


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
