module Main where

import Polynomial(..)
import Graphics.Collage(..)
import Color

binTree : Polynomial
binTree = sum [unit, product [var, var]]

natList : Polynomial
natList = sum [unit, product [Const "Nat", var]]

twoThreeTree : Polynomial
twoThreeTree = sum [unit, product [var,var], product [var,var,var]]

nat : Polynomial
nat = sum [unit, var]

drawing = drawMu 4 800 nat
-- drawing = juxt 100 (filled Color.green (square 100)) (filled Color.red (square 100))

main = collage 800 800 [drawing]

