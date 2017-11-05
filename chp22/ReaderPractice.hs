module ReaderPractice where

import Data.Monoid
import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- if' :: Bool -> a -> a -> a
-- if' a b c = if a then b else c

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- lookup a = getFirst . foldMap (First . maybeMatch)
--             where maybeMatch x = if' (fst x == a) (Just $ snd x) Nothing

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' = flip lookup $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = fmap (,) xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = fmap (,) ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ getAll $ foldMap All $ sequA 2
  print $ sequA $ fromMaybe 0 s'
  print $ sequA $ fromMaybe 0 ys
  print $ (bolt . fromMaybe 0) <$> z' $ 2
