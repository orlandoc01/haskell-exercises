module StateClasses where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ (,) <$> (f . fst) <*> snd <$> g

instance Applicative (Moi s) where
  pure a = Moi $ (,) a
  (<*>) (Moi sAb) (Moi sA) = Moi $ \s -> 
      let (a, nextState) = sA s
          (faB, finalState) = sAb nextState
      in (faB a, finalState)

instance Monad (Moi s) where
  return = pure
  (>>=) (Moi sA) fMoiSB = Moi $ \s ->
    let (a, nextState) = sA s
    in runMoi (fMoiSB a) nextState
