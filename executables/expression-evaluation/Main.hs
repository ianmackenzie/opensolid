module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.IO qualified as IO
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Quantity (Quantity)
import OpenSolid.Syntax (int, number, (.+), (./))
import OpenSolid.Text qualified as Text
import OpenSolid.Units (Unitless)

main :: IO ()
main = do
  let t = Expression.t

  IO.printLine "t^2 / (1 + t^2)"
  let tSquared = Expression.squared t
  let one :: Expression (Quantity Unitless) (Quantity Unitless) = Expression.constant 1.0
  let fraction = tSquared ./ (one .+ tSquared)
  IO.forEach [int 0 .. 5] \i -> do
    let evaluated = Expression.evaluate fraction (number (fromIntegral i))
    IO.printLine (Text.number evaluated)

  IO.printLine "Bezier curve"
  let bezier = Expression.bezierCurve (0.0 :| [0.5, 1.0, 1.0, 1.0, 1.0, 0.5, 0.0])
  IO.forEach (Parameter.steps 10) \tValue -> do
    let evaluated = Expression.evaluate bezier tValue
    IO.printLine (Text.number evaluated)
  IO.forEach (Parameter.intervals 10) \tBounds -> do
    let evaluated = Expression.evaluateBounds bezier tBounds
    IO.printLine (Text.show evaluated)
