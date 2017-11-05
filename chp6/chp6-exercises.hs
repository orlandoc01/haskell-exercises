module TypeClass where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk _ _ _ = True
