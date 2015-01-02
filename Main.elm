module Main where

import Polynomial(..)
import Graphics.Collage(..)
import Color

binTree : Polynomial
binTree = Add (Const "1") (Mul (Var ()) (Var ()))

natList : Polynomial
natList = Add (Const "1") (Mul (Const "Nat") (Var ()))

twoThreeTree : Polynomial
twoThreeTree = unit +: (var *: var) +: (var *: var *: var)

nat : Polynomial
nat = Add (Const "1") (Var ())

drawing = drawMu 2 800 twoThreeTree
-- drawing = juxt 100 (filled Color.green (square 100)) (filled Color.red (square 100))

main = collage 800 800 [drawing]

