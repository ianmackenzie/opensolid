module Expect
  ( Expectation (Passed, Failed)
  , expect
  , all
  , list
  , approximatelyEqual
  , rangeApproximatelyIncludes
  )
where

import OpenSolid
import Range (Range)
import Range qualified

data Expectation
  = Passed
  | Failed Text

instance (ErrorMessage x, a ~ a') => Bind (Result x a) a' Expectation where
  bind f (Ok value) = f value
  bind _ (Error error) = Failed (errorMessage error)

expect :: Bool -> Text -> Expectation
expect True _ = Passed
expect False failureMessage = Failed failureMessage

approximatelyEqual :: (Tolerance units, ApproximateEquality a b units) => a -> b -> Expectation
approximatelyEqual expected actual =
  expect (expected ~= actual) "Given values are not approximately equal"

rangeApproximatelyIncludes :: Tolerance units => Qty units -> Range units -> Expectation
rangeApproximatelyIncludes value range =
  expect (Range.approximatelyIncludes value range) "Given value is not contained in the given range"

all :: List Expectation -> Expectation
all [] = Passed
all (first : rest) = both first (all rest)

both :: Expectation -> Expectation -> Expectation
both Passed Passed = Passed
both (Failed message) Passed = Failed message
both Passed (Failed message) = Failed message
both (Failed message1) (Failed message2) = Failed (message1 ++ "\n" ++ message2)

list :: (a -> a -> Expectation) -> List a -> List a -> Expectation
list _ [] [] = Passed
list _ (_ : _) [] = Failed "List lengths do not match"
list _ [] (_ : _) = Failed "List lengths do not match"
list expectation (head1 : tail1) (head2 : tail2) =
  both (expectation head1 head2) (list expectation tail1 tail2)
