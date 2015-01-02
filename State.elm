module State ( State
             , return
             , join
             , map
             , bind
             , (>!=)
             , run
             , eval
             , exec
             , get
             , put
             , modify
             , mapM
             ) where

import List((::))

type alias State s a = s -> (a, s)

get : State s s
get = \s -> (s, s)

put : s -> State s ()
put s' = \_ -> ((), s')

modify : (s -> s) -> State s ()
modify f = \s -> ((), f s)

eval : State s a -> s -> a
eval mx s = fst (run mx s)

exec : State s a -> s -> s
exec mx s = snd (run mx s)

run : State s a -> s -> (a, s)
run mx = mx

return : a -> State s a
return x = \s -> (x, s)

join : State s (State s a) -> State s a
join f = \s -> let (mx, s') = f s in mx s'

map : (a -> b) -> State s a -> State s b
map f mx = \s -> let (x, s') = mx s in (f x, s')

bind : (a -> State s b) -> State s a -> State s b
bind f mx = \s -> let (x, s') = mx s in f x s'

-- A non-principal type was inferred for this, causing wonky errors
(>!=) : State s a -> (a -> State s b) -> State s b
(>!=) mx f = bind f mx

mapM : (a -> State s b) -> List a -> State s (List b)
mapM f xs = case xs of
  []       -> return []
  x :: xs' -> f x >!= \y -> map ((::) y) (mapM f xs')

