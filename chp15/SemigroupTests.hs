module SemigroupTests where
import Control.Monad
import Data.Semigroup
import Data.Monoid hiding ((<>))
import Test.QuickCheck hiding (Success, Failure)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mappend a mempty == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = mappend mempty a == a

---------- Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)
 
instance Arbitrary Trivial where
  arbitrary = return Trivial

---------- Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity $ x <> y

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mappend = (<>)
  mempty = Identity (mempty :: Monoid a => a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a
  
---------- Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two w x) <> (Two y z) = Two (w <> y) (x <> z)

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  mappend = (<>)
  mempty = Two (mempty :: Monoid a => a) (mempty :: Monoid b => b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

---------- BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Monoid BoolConj where
  mappend = (<>)
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

---------- Or
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  x <> y = case (x, y) of (Snd a, _) -> x; (_, Snd b) -> y; (_, _) -> y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ if c then Fst a else Snd b

---------- Combine
newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  x <> y = Combine $ unCombine x <> unCombine y

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mappend = (<>)
  mempty = Combine $ const (mempty :: Monoid b => b)

---------- Comp
newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
  x <> y = Comp $ unComp x . unComp y

instance Monoid (Comp a) where
  mappend = (<>)
  mempty = Comp id

--------- Validation
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  x <> y = case (x, y) of
    (Failure w, Failure v) -> Failure $ w <> v
    ( _, Success v) -> Success v
    (Success w, _ ) -> Success w

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return (if z then Failure x else Success y)

--------- AccumulateRight
newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight x) <> (AccumulateRight y) = case (x, y) of
    (Success w, Success v) -> AccumulateRight $ Success $ w <> v
    ( _, Failure v) -> AccumulateRight $ Failure v
    (Failure w, _ ) -> AccumulateRight $ Failure w

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    x <- arbitrary
    return $ AccumulateRight x

---------- AccumulateBoth
newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth x) <> (AccumulateBoth y) = case (x, y) of
    (Success w, Success v) -> AccumulateBoth $ Success $ w <> v
    (Failure w, Failure v) -> AccumulateBoth $ Failure $ w <> v
    (Success w, _) -> AccumulateBoth $ Success w
    (_, Success v) -> AccumulateBoth $ Success v

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    x <- arbitrary
    return $ AccumulateBoth x

---------- Semigroup and Monoid Tests
type AssocProp a = a -> a -> a -> Bool
type LeftProp a = a -> Bool
type RightProp a = a -> Bool
tests :: IO ()
tests = do
  quickCheck (semigroupAssoc :: AssocProp Trivial)
  quickCheck (monoidLeftIdentity :: LeftProp Trivial)
  quickCheck (monoidRightIdentity :: RightProp Trivial)
  quickCheck (semigroupAssoc :: AssocProp (Identity String))
  quickCheck (monoidLeftIdentity :: LeftProp (Identity String))
  quickCheck (monoidRightIdentity :: RightProp (Identity String))
  quickCheck (semigroupAssoc :: AssocProp (Two String String))
  quickCheck (monoidLeftIdentity :: LeftProp (Two String String))
  quickCheck (monoidRightIdentity :: RightProp (Two String String))
  quickCheck (semigroupAssoc :: AssocProp BoolConj)
  quickCheck (monoidLeftIdentity :: LeftProp BoolConj)
  quickCheck (monoidRightIdentity :: RightProp BoolConj)
  quickCheck (semigroupAssoc :: AssocProp (Or String String))
  quickCheck (semigroupAssoc :: AssocProp (Validation String String))
  quickCheck (semigroupAssoc :: AssocProp (AccumulateRight String String))
  quickCheck (semigroupAssoc :: AssocProp (AccumulateBoth String String))


newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty :: Monoid b => b, s)
  mappend (Mem f) (Mem g) = Mem $ \s ->
    let firstApp = f s
        sndApp = g $ snd firstApp
    in (mappend (fst firstApp) (fst sndApp), snd sndApp)


f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

main = do
  print $ runMem (mappend f' mempty) 0
  print $ runMem (mappend mempty f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (mappend f' mempty) 0 == runMem f' 0
  print $ runMem (mappend mempty f') 0 == runMem f' 0
