module Main (main) where

import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

main :: IO ()
main = IO.do
  let t = Expression.t

  IO.printLine "t^2 / (1 + t^2)"
  let tSquared = Expression.squared t
  let one :: Expression Float Float = Expression.constant 1.0
  let fraction = tSquared / (one + tSquared)
  IO.forEach [0 .. 5] \i -> IO.do
    let evaluated = Expression.evaluate fraction (Float.int i)
    IO.printLine (Text.float evaluated)

  IO.printLine "Bezier curve"
  let bezier = Expression.bezierCurve (NonEmpty.eight 0.0 0.5 1.0 1.0 1.0 1.0 0.5 0.0)
  IO.forEach (Parameter.steps 10) \tValue -> IO.do
    let evaluated = Expression.evaluate bezier tValue
    IO.printLine (Text.float evaluated)
  IO.forEach (Parameter.intervals 10) \tBounds -> IO.do
    let evaluated = Expression.evaluateBounds bezier tBounds
    IO.printLine (Text.show evaluated)
