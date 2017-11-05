module Chp23 where
import Control.Monad.State hiding (get, put, exec, eval, modify)

get :: State s s
get = state $ (,) <*> id

put :: s -> State s ()
put = state . const . (,) ()

exec :: State s a -> s -> s
exec = (snd .) . runState

eval :: State s a -> s -> a
eval = (fst .) . runState

modify :: (s -> s) -> State s ()
modify f = state $ (,) () . f
