module MonadsGalore where
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Nope
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

-- PhhhbbtttEither
data PhhhbbtttEither b a = Left1 a | Right1 b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f x = case x of (Left1 a) -> Left1 $ f a; (Right1 b) -> Right1 b

instance Applicative (PhhhbbtttEither b) where
  pure = Left1
  (<*>) x y = case (x, y) of
    (Left1 f, Left1 a) -> Left1 $ f a
    (Right1 b, _) -> Right1 b
    (_, Right1 b) -> Right1 b

instance Monad (PhhhbbtttEither b) where
  return = Left1
  (>>=) x y = case (x, y) of
    (Right1 a, _) -> Right1 a
    (Left1 a, f) -> f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither a b) where
  arbitrary = arbitrary >>= \x -> if x then fmap Left1 arbitrary else fmap Right1 arbitrary

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither a b) where (=-=) = eq

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = pure Identity <*> arbitrary

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f a = case a of
    Nil -> Nil
    Cons a as -> Cons (f a) (fmap f as)
 
instance Monoid (List a) where
  mempty = Nil
  mappend x y = case (x,y) of
    (a, Nil) -> a
    (Nil, b) -> b
    (Cons a as, bs) -> Cons a (mappend as bs)
 
instance Applicative List where
  pure a = Cons a Nil
  (<*>) fs as = case (fs, as) of
    (Nil, _) -> Nil
    (Cons f fs, as) -> fmap f as <> (fs <*> as)
  
instance Monad List where
  return a = Cons a Nil
  (>>=) xs f = case (xs, f) of
    (Nil, _) -> Nil
    (Cons a as, f) -> f a <> (as >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fmap (foldr Cons Nil) (arbitrary :: Arbitrary a => Gen [a])

instance Eq a => EqProp (List a) where (=-=) = eq

---------- Functions
--fmap return >>=
j :: Monad m => m (m a) -> m a
j = flip (>>=) id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = pure f <*> a <*> b

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh as f = case (as, f) of
  ([], _) -> pure []
  (a:as, f) -> (:) <$> f a <*> meh as f

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id
