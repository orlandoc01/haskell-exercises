{-# LANGUAGE ViewPatterns #-}
module Functors1 where
import Test.QuickCheck
import Test.QuickCheck.Function


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g .f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = fmap (g . f) x == (fmap g . fmap f $ x)


-- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- Pair
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

-- Two
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
 
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c
  
-- Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

-- Four
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

-- Possibly
data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f possibly = case possibly of (Yeppers a) -> Yeppers $ f a; LolNope -> LolNope

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ if b then Yeppers a else LolNope

-- Sum
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f sum = case sum of (Second b) -> Second $ f b; First a -> First a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ if c then First a else Second b

-- Tests
type IntToInt = Fun Int Int
type IntFunctorCompose f = f Int -> IntToInt -> IntToInt -> Bool
type IntFunctorIdentity f = f Int -> Bool
main :: IO ()
main = do
  quickCheck (functorCompose' :: IntFunctorCompose Identity)
  quickCheck (functorIdentity :: IntFunctorIdentity Identity)
  quickCheck (functorCompose' :: IntFunctorCompose Pair)
  quickCheck (functorIdentity :: IntFunctorIdentity Pair)
  quickCheck (functorCompose' :: IntFunctorCompose (Two Int))
  quickCheck (functorIdentity :: IntFunctorIdentity (Two Int))
  quickCheck (functorCompose' :: IntFunctorCompose (Three' Int))
  quickCheck (functorIdentity :: IntFunctorIdentity (Three' Int))
  quickCheck (functorCompose' :: IntFunctorCompose (Four Int Int Int))
  quickCheck (functorIdentity :: IntFunctorIdentity (Four Int Int Int))
  quickCheck (functorCompose' :: IntFunctorCompose (Four' Int))
  quickCheck (functorIdentity :: IntFunctorIdentity (Four' Int))
  quickCheck (functorCompose' :: IntFunctorCompose Possibly)
  quickCheck (functorIdentity :: IntFunctorIdentity Possibly)
  quickCheck (functorCompose' :: IntFunctorCompose (Sum Int))
  quickCheck (functorIdentity :: IntFunctorIdentity (Sum Int))

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

