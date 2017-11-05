module Apl1 where

import Control.Applicative
import Data.Monoid (Monoid, (<>))
import Test.QuickCheck (Arbitrary, arbitrary, Gen)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend a b = case (a, b) of
    (Nil, b) -> b
    (a, Nil) -> a
    (Cons a as, bs) -> Cons a (mappend as bs)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)
 
instance Applicative List where
  pure a = Cons a Nil
  a <*> b = case (a, b) of
    (Nil, _) -> Nil
    (_, Nil) -> Nil
    (Cons f fs, as) -> mappend (fmap f as) (fs <*> as)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary :: Arbitrary a => Gen [a]
    return $ foldr (\x acc -> mappend (pure x) acc) Nil x

instance Eq a => EqProp (List a) where (=-=) = eq

take' :: Int -> List a -> List a
take' i l = case (i, l) of 
  (_, Nil) -> Nil
  (0, _) -> Nil
  (i, Cons a as) -> Cons a $ take' (i - 1) as

infinite' :: a -> List a
infinite' a = Cons a (infinite' a)

--- ZipList '
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

getList :: ZipList' a -> List a
getList (ZipList' l) = l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = pure ZipList' <*> arbitrary
  
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where 
      xs' = let (ZipList' l) = xs
            in take' 3000 l
      ys' = let (ZipList' l) = ys
            in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' $ infinite' a
  (<*>) (ZipList' l1) (ZipList' l2) = case (l1, l2) of
    (_, Nil) -> ZipList' Nil
    (Nil, _ ) -> ZipList' Nil
    (Cons f fs, Cons a as) -> ZipList' $ Cons (f a) $ getList (ZipList' fs <*> ZipList' as)
  
-- Sum
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f a = case a of (First b) -> First b; (Second b) -> Second $ f b

instance Applicative (Sum a) where
  pure = Second
  (<*>) a b = case (a, b) of
    (First a, _) -> First a
    (_, First a) -> First a
    (Second f, Second a) -> Second $ f a

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ if c then First a else Second b

-- Validation Type
data Validation e a = Error e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f a = case a of (Error e) -> Error e; (Success a) -> Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) a b = case (a, b) of
    (Error a, Error b) -> Error $ a <> b
    (Error a, _) -> Error a
    (_, Error b) -> Error b
    (Success f, Success b) -> Success $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ if c then Error a else Success b

instance (Eq a, Eq b) => EqProp (Validation a b) where (=-=) = eq

-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = pure Identity <*> arbitrary

instance (Eq a) => EqProp (Identity a) where (=-=) = eq


-- Pair
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f g) (Pair a b) = Pair (f a) (g b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = pure Pair <*> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Pair a) where (=-=) = eq

-- Two
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
  pure = Two mempty 
  (<*>) (Two a f) (Two b x) = Two (a <> b) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = pure Two <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

-- Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a x y) = Three' a (f x) (f y)

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a f g) (Three' b x y) = Three' (a <> b) (f x) (g y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = pure Three' <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- Four'
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c x) = Four' a b c (f x)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty 
  (<*>) (Four' a b c f) (Four' a1 b1 c1 x) = Four' (a <> a1) (b <> b1) (c <> c1) (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = pure Four' <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

---------------------------------
stops = "pbtdkg"
vowels = "aeiou"

combos:: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

