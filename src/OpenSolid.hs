module OpenSolid
  ( module Basics
  , module Concatenate
  , module Arithmetic
  , module Result
  , module Coalesce
  , module Qty
  , module Float
  , module List
  , module Sign
  , module Task
  , module DoNotation
  , module CoordinateSystem
  , Indeterminate (Indeterminate)
  , Tolerance
  , ApproximateEquality ((~=))
  , print
  )
where

import Arithmetic
import Basics
import Coalesce (Coalesce ((??)))
import Concatenate (Concatenate ((++)))
import CoordinateSystem (CoordinateSystem, type (@))
import Data.Text.IO qualified
import DoNotation
import Float (Float, fromRational)
import List (List)
import Qty (Qty (..))
import Result (IsError (errorMessage), Result (Error, Ok))
import Sign (Sign (Negative, Positive))
import Task (Task)
import Task qualified
import Prelude qualified

data Indeterminate = Indeterminate

instance IsError Indeterminate where
  errorMessage Indeterminate = "Result is indeterminate"

type Tolerance units = ?tolerance :: Qty units

class ApproximateEquality a b units | a -> units, b -> units where
  (~=) :: Tolerance units => a -> b -> Bool

infix 4 ~=

instance units ~ units' => ApproximateEquality (Qty units) (Qty units') units where
  x ~= y = let (Qty delta) = x - y in Qty (Prelude.abs delta) <= ?tolerance

print :: Text -> Task Text ()
print text =
  Task.perform (Data.Text.IO.putStrLn text)
    |> Task.mapError errorMessage
