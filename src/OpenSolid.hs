module OpenSolid
  ( module Basics
  , module Concatenate
  , module Arithmetic
  , module Result
  , module Qty
  , module Float
  , module List
  , module Sign
  , module Task
  , module DoNotation
  , module CoordinateSystem
  , Tolerance
  , ApproximateEquality ((~=))
  , print
  )
where

import Arithmetic
import Basics
import Concatenate (Concatenate ((++)))
import CoordinateSystem (CoordinateSystem, type (@))
import Data.Text.IO qualified
import DoNotation
import Float (Float, fromRational)
import List (List)
import Qty (Qty (..))
import Qty qualified
import Result (Indeterminate (Indeterminate), IsError (errorMessage), Result (Error, Ok))
import Sign (Sign (Negative, Positive))
import Task (Task)
import Task qualified

type Tolerance units = ?tolerance :: Qty units

class ApproximateEquality a b units | a -> units, b -> units where
  (~=) :: Tolerance units => a -> b -> Bool

infix 4 ~=

instance units ~ units' => ApproximateEquality (Qty units) (Qty units') units where
  x ~= y = Qty.abs (x - y) <= ?tolerance

print :: Text -> Task Text ()
print text = Task.fromIO (Data.Text.IO.putStrLn text) |> Task.mapError errorMessage
