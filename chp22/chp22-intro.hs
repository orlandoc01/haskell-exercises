module Chp22Intro where

import Control.Applicative
import Data.Char

-- Exercises
cap :: String -> String
cap = map toUpper 

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

tupled :: String -> (String, String)
tupled = (,) <$> rev <*> cap

tupled' :: String -> (String, String)
tupled' = do
  a <- rev
  b <- cap
  return $ (,) a b

tupled'' :: String -> (String, String)
tupled'' = rev >>= \a -> cap >>= \b -> return $ (,) a b

-- (,) <$> rev == \a -> \b -> (,) (rev a) b
-- cap == \a -> cap a
-- (,) <$> rev <*> == \a -> (,) (rev a) (cap a)
----------------Reader
newtype Reader r a = Reader { runReader :: r -> a }
ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap f (Reader r) = Reader $ f . r

instance Applicative (Reader r) where
  pure a = Reader $ const a
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) (Reader ra) aRb = Reader $ \r -> runReader (aRb $ ra r) r

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName,
  dogName :: DogName,
  address :: Address
} deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName,
    dogsAddress :: Address
} deriving (Eq, Show)

person = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sessame Street")
chris = Person (HumanName "Chris Allen")  (DogName "Papu")  (Address "Austin")

getDogR :: Reader Person Dog
getDogR = Reader $ Dog <$> dogName <*> address

getDogRM :: Reader Person Dog
getDogRM = Reader dogName >>= (\name -> Reader $ Dog name . address)
