module Poly where

type Poly
  = Var
  | Const String
  | Add Poly Poly
  | Mul Poly Poly

