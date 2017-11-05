module Fibs where

-- fibs = 1 : scanl (+) 1 fibs

fibs 
  | length fibs == 20 = fibs 
  | otherwise = 1 : scanl (+) 1 fibs


1 : (scanl (+) 1 (1: scanl (+) 1 (1: scanl (+) 1 [])))
1 : (scanl (+) 1 (1: scanl (+) 1 ([1,1])))
