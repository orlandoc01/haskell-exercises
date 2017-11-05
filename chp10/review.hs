module Chp10Review where

stops = "pbtdkg"
vowels = "aeiou"

allCombos = [ (x,y,z) | x <- stops, y <- vowels, z <- stops]
allComboPs = [ (x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']


myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = foldr (\x acc -> acc || f x) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any ((==) a)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl addMax x (x:xs)
                       where addMax val acc
                              | f val acc == GT = val
                              | f val acc == EQ = val
                              | f val acc == LT = acc
