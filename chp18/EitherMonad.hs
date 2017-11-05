module EitherMonad where

type Founded = Int
type Coders = Int

data SoftwareShop = 
  Shop { 
    founded :: Founded, 
    programmers :: Coders
  } deriving (Eq, Show)

data FoundedError = 
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 500 = Left $ TooManyCoders n
  | otherwise = Right n

validateShop :: SoftwareShop -> Either FoundedError SoftwareShop
validateShop (Shop f c)
  | c > div f 10 = Left $ TooManyCodersForYears f c
  | otherwise = Right $ Shop f c

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware f c = pure Shop <*> validateFounded f <*> validateCoders c >>= validateShop

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f a = case a of (Second x) -> Second $ f x; (First y) -> First y

instance Applicative (Sum a) where
  pure = Second
  (<*>) a b = case (a, b) of
    (Second f, Second x) -> Second $ f x
    (First y, _) -> First y
    (_, First y) -> First y

instance Monad (Sum a) where
  return = pure
  (>>=) a f = case (a, f) of
    (First y, _) -> First y
    (Second x, f) -> case f x of 
      (First y) -> First y
      (Second x) -> Second x
              
      
