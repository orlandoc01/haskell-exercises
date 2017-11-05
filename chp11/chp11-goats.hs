module Chp11Goats where
{-# LANGUAGE GeneralizedNewTypeDeriving #-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- instance TooMany (Int, String) where
  
