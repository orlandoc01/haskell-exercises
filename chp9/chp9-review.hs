module Chp9Revew where
import Data.Char

findCapital :: String -> String
findCapital = filter isUpper

capitalizeFirst :: String -> String
capitalizeFirst "" = ""
capitalizeFirst (x:xs) = toUpper x : capitalizeFirst xs

grabFirst :: String -> Char
grabFirst = toUpper . head

charShift :: Int -> Char -> Char
charShift x c = chr $ 97 + mod (ord c - 97 + x)  26

encodeCeasar :: Int -> String -> String
encodeCeasar x = map $ charShift x

unCeasar :: Int -> String -> String
unCeasar x = encodeCeasar $ negate x

encodeCeasarVaried :: [Int] -> String -> String
encodeCeasarVaried xs s = map (uncurry charShift) $ zip (cycle xs) s

unCeasarVaried :: [Int] -> String -> String
unCeasarVaried xs = encodeCeasarVaried $ map negate xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if (f x) == True then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny $ (==) a

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs)  = myReverse xs ++ [x]

squish :: [[a]] ->[a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
