module FoldableStuff where
import Data.Monoid
import Data.Semigroup hiding ((<>))

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = getAny . foldMap (Any . (==) a)

minimum :: (Foldable t, Ord a, Bounded a) => t a -> a
minimum = getMin . foldMap Min

maximum :: (Foldable t, Ord a, Bounded a) => t a -> a
maximum = getMax . foldMap Max

length :: Foldable t => t a -> Int
length = getSum . foldMap (const 1)

toList :: (Foldable t) => t a -> [a]
toList = foldMap $ flip (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x acc -> f x <> acc) mempty

newtype Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap _ _ = mempty
 
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two a b) = f b

data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = f b <> f c

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' a b c d) = f b <> f c <> f d

filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF p = foldMap (\x -> if p x then pure x else mempty)
