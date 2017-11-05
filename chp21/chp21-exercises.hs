module TraversableStuff where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) = fmap f

instance Foldable Identity where
  foldMap f (Identity a) = f a
  
instance Traversable Identity where
  sequenceA (Identity f) = fmap Identity f

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- Constant
newtype Constant a b = Constant { getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid m => Applicative (Constant m) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant $ a <> b

instance Monoid m => Foldable (Constant m) where
  foldMap _ (Constant m) =  mempty
 
instance Monoid m => Traversable (Constant m) where
  sequenceA (Constant a) = pure $ Constant a
  
instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance Eq a => EqProp (Constant a b) where (=-=) = eq

-- Optional
data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap f a = case a of Nada -> Nada; (Yep a) -> Yep $ f a

instance Applicative Optional where
  pure = Yep
  (<*>) a b = case (a,b) of
    (Nada, _) -> Nada
    (Yep f, a) -> fmap f a
 
instance Foldable Optional where
  foldMap f a = case a of Nada -> mempty; (Yep a) -> f a

instance Traversable Optional where
  sequenceA f = case f of Nada -> pure Nada; (Yep f) -> fmap Yep f

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, fmap Yep arbitrary]

instance Eq a => EqProp (Optional a) where (=-=) = eq

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap f xs = case xs of Nil -> Nil; (Cons a as) -> Cons (f a) (fmap f as)

instance Monoid (List a) where
  mempty = Nil
  mappend xs ys = case (xs, ys) of
    (Nil, bs) -> bs
    (Cons a as, bs) -> Cons a (mappend as bs)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) xs ys = case (xs, ys) of
    (Nil, _) -> Nil
    (Cons f fs, as) -> fmap f as <> (fs <*> as)

instance Foldable List where
  foldMap f xs = case xs of
    Nil -> mempty
    (Cons a as) -> f a <> foldMap f as

instance Traversable List where
  sequenceA fs = case fs of
    Nil -> pure Nil
    Cons f fs -> pure mappend <*> fmap pure f <*> sequenceA fs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap (foldr Cons Nil) (arbitrary :: Arbitrary a => Gen [a])

instance Eq a => EqProp (List a) where (=-=) = eq

-- Three
data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three c d x) = Three (a <> c) (b <> d) (f x)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance (Monoid a, Monoid b) => Traversable (Three a b) where
  sequenceA (Three a b f) = fmap (Three a b) f

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = fmap Three arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- Three'
data Three' a b = Three' a b b deriving (Eq, Ord, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  (<*>) (Three' a f g) (Three' b x y) = Three' (a <> b) (f x) (g y)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> f c
  
instance Monoid a => Traversable (Three' a) where
  sequenceA (Three' a f g) = fmap (Three' a) f <*> g

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = fmap Three' arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- S
data S n a = S (n a) a deriving (Eq, Ord, Show)

instance Functor f => Functor (S f) where
  fmap f (S g a) = S (fmap f g) (f a)

instance (Arbitrary (f b), Arbitrary b) => Arbitrary (S f b) where
  arbitrary = fmap S arbitrary <*> arbitrary

instance (Applicative f) => Applicative (S f) where
  pure a = S (pure a) a
  (<*>) (S f a) (S g b) = S (f <*> g) (a b)

instance Foldable f => Foldable (S f) where
  foldMap f (S g a) = foldMap f g <> f a

instance Traversable t => Traversable (S t) where
  sequenceA (S f g) = fmap S (sequenceA f) <*> g

instance (Eq (f a), Eq a) => EqProp (S f a) where (=-=) = eq

-- Tree

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show)

insert :: (Eq a, Ord a) => a -> Tree a -> Tree a
insert v tree = case tree of
  Empty -> Node Empty v Empty
  Leaf a -> if v > a then Node Empty a (Leaf v) else Node (Leaf v) a Empty
  Node l a r -> if v > a then Node l a (insert v r) else Node (insert v l) a r

instance Functor Tree where
  fmap f t = case t of
    Empty -> Empty
    Leaf a -> Leaf $ f a
    Node l a r -> Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap f tree = case tree of
    Empty -> mempty
    Leaf a -> f a
    Node l a r -> foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  sequenceA fs = case fs of
    Empty -> pure Empty
    Leaf f -> fmap Leaf f
    Node l a r -> fmap Node (sequenceA l) <*> a <*> sequenceA r

instance (Arbitrary a, Ord a) => Arbitrary (Tree a) where
  arbitrary = fmap (foldr insert Empty) (arbitrary :: Arbitrary a => Gen [a])

instance Eq a => EqProp (Tree a) where (=-=) = eq

-- Compose
newtype Compose f g x = Compose (f (g x)) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose val) = Compose $ (fmap . fmap) f val
 
instance (Arbitrary (f (g x)), Arbitrary (g x), Arbitrary x) => Arbitrary (Compose f g x) where
  arbitrary = fmap Compose arbitrary

instance (Eq (f (g x)), Eq (g x), Eq x) => EqProp (Compose f g x) where (=-=) = eq
