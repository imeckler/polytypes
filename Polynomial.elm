module Polynomial ( Poly(..)
                  , Polynomial
                  , drawMu
                  , juxt
                  , atop
                  ) where

import Graphics.Collage(..)
import Color
import Text
import Transform2D
import Array (Array)
import Array
import State
import State((>!=))
import Debug

type Poly a
  = Var a
  | Const String
  | Add (Poly a) (Poly a)
  | Mul (Poly a) (Poly a)

map : (a -> b) -> Poly a -> Poly b
map f p =
  case p of
    Const _   -> p
    Add p1 p2 -> Add (map f p1) (map f p2)
    Mul p1 p2 -> Mul (map f p1) (map f p2)
    Var x     -> Var (f x)

type alias Polynomial = Poly ()

textForm : String -> Form
textForm = toForm << Text.leftAligned << Text.fromString

scaleMatrix x y = Transform2D.matrix x 0 0 y 0 0
scaleSep x y f = groupTransform (scaleMatrix x y) [f]

atop h a b =
  let m = scaleMatrix 1 (1/2)
  in group [moveY (-h/2) (groupTransform m [a]), moveY (h/2) (groupTransform m [b])]

juxt w a b =
  let m = scaleMatrix (1/2) 1
  in group [moveX (-w/2) (groupTransform m [a]), moveX (w/2) (groupTransform m [b])]

(!) : Array a -> Int -> a
(!) a i = case Array.get i a of
  Just x -> x
  Nothing -> Debug.crash (toString a)

mulSeparator len = traced (dashed Color.black) [(-len/2, 0), (len/2, 0)]
addSeparator len = group [filled Color.green (rect 3 len), filled Color.black (rect 10 3)]

drawMu : Int -> Float -> Polynomial -> Form
drawMu maxDepth len p =
  let boring d = filled Color.white (square len)
      mulSep = mulSeparator len
      addSep = addSeparator len
      levelCached t mem = Array.length mem > t

      -- Add is juxt. Mul is atop.
      go t q =
        case q of
          Const s   -> State.return <| group [filled Color.white (square len), textForm s]
          Var ()    ->
            State.get >!= \mem ->
              if t < 0
              then State.return (boring t)
              else 
                if t < Array.length mem
                then State.return (mem ! t)
                else go (t - 1) p >!= \r -> State.map (\_ -> r) (State.modify (Array.push r))

          {- This correct code caused the compiler to stack overflow (when (>!=) had no type annotation in State.elm)
          Var ()    ->
            State.get >!= \mem ->
              if Array.length mem > d
              then State.return (mem ! d)
              else go (d + 1) p >!= \r -> State.map (\_ -> r) (State.modify (Array.push r))
          -}

          Mul p1 p2 -> 
            go t p1 >!= \r1 ->
              go t p2 >!= \r2 ->
                State.return <| group [atop len r1 r2, mulSep]

          Add p1 p2 ->
            go t p1 >!= \r1 ->
              go t p2 >!= \r2 ->
                State.return <| group [juxt len r1 r2, addSep]
  in
  State.eval (go 5 p) Array.empty

drawPoly : Poly Form -> Form
drawPoly p =
  case p of 

adopt : Polynomial -> Form -> Form
adopt 

{-

drawPoly : Polynomial -> Form

adopt p (drawPoly q) = drawPoly (subst p q)

subst : Polynomial -> Polynomial -> Polynomial
-}