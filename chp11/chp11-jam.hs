module Jammin where
import Data.List

data Fruit = Peach | Plum | Apple | Blackberry deriving (Eq, Show, Ord)
data JamJars = Jam { fruit :: Fruit, jars :: Int } deriving (Eq, Show, Ord)

sumJars :: [JamJars] -> Int
sumJars js = sum $ map jars js

mostRow :: [JamJars] -> JamJars
mostRow = foldr higherRow (Jam Peach 0)
  where higherRow row1 row2 
          | jars row1 > jars row2 = row1
          | otherwise = row2

sortJams :: [JamJars] -> [JamJars]
sortJams = sortBy compare

