module Chp7 where 

foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y test
  | test == True  = x
  | test == False = y

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y test =
  case test of
    True -> x
    False -> y
