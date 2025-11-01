module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.IO qualified as IO
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Qty (Qty)
import OpenSolid.Syntax (float, int, (.+), (./))
import OpenSolid.Text qualified as Text
import OpenSolid.Units (Unitless)

main :: IO ()
main = IO.do
  let t = Expression.t

  IO.printLine "t^2 / (1 + t^2)"
  let tSquared = Expression.squared t
  let one :: Expression (Qty Unitless) (Qty Unitless) = Expression.constant 1.0
  let fraction = tSquared ./ (one .+ tSquared)
  IO.forEach [int 0 .. 5] \i -> IO.do
    let evaluated = Expression.evaluate fraction (float (fromIntegral i))
    IO.printLine (Text.float evaluated)

  IO.printLine "Bezier curve"
  let bezier = Expression.bezierCurve (0.0 :| [0.5, 1.0, 1.0, 1.0, 1.0, 0.5, 0.0])
  IO.forEach (Parameter.steps 10) \tValue -> IO.do
    let evaluated = Expression.evaluate bezier tValue
    IO.printLine (Text.float evaluated)
  IO.forEach (Parameter.intervals 10) \tBounds -> IO.do
    let evaluated = Expression.evaluateBounds bezier tBounds
    IO.printLine (Text.show evaluated)
