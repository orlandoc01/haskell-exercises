module Chp15 where
import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend x y = case (x,y) of 
                  (Nada, w) -> y
                  (v, Nada) -> v
                  (Only v, Only w) -> Only $ v <> w
