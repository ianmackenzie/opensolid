module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.IO qualified as IO
import OpenSolid.Number (Number)
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude ((.+.), (./.))
import OpenSolid.Text qualified as Text

main :: IO ()
main = do
  let t = Expression.t

  IO.printLine "t^2 / (1 + t^2)"
  let tSquared = Expression.squared t
  let one :: Expression Number Number = Expression.constant 1
  let fraction = tSquared ./. (one .+. tSquared)
  IO.forEach [0 :: Int .. 5] \i -> do
    let evaluated = Expression.evaluate fraction (fromIntegral i)
    IO.printLine (Text.number evaluated)

  IO.printLine "Bezier curve"
  let bezier = Expression.bezierCurve (0 :| [0.5, 1, 1, 1, 1, 0.5, 0])
  IO.forEach (Parameter.steps 10) \tValue -> do
    let evaluated = Expression.evaluate bezier tValue
    IO.printLine (Text.number evaluated)
  IO.forEach (Parameter.intervals 10) \tBounds -> do
    let evaluated = Expression.evaluateBounds bezier tBounds
    IO.printLine (Text.show evaluated)
