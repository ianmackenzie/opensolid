module OpenSolid.Differentiable (Differentiable (derivative)) where

class
  Differentiable parameter function derivative
    | function -> parameter
    , function -> derivative
  where
  derivative :: parameter -> function -> derivative
