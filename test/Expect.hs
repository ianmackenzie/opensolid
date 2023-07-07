module Expect
  ( Expectation (Passed, Failed)
  , pass
  , fail
  , expect
  , all
  , list
  , pair
  , equal
  , approximately
  , rangeApproximatelyIncludes
  , range
  )
where

import OpenSolid hiding (fail)
import Range (Range (Range))
import Range qualified

data Expectation
  = Passed
  | Failed Text

instance (ErrorMessage x, a ~ a') => Bind (Result x a) a' Expectation where
  bind f (Ok value) = f value
  bind _ (Error error) = Failed (errorMessage error)

pass :: Expectation
pass = Passed

fail :: Text -> Expectation
fail = Failed

expect :: Bool -> Text -> Expectation
expect True _ = Passed
expect False failureMessage = Failed failureMessage

equal :: Eq a => a -> a -> Expectation
equal expected actual =
  expect (expected == actual) "Given values are not equal"

approximately :: (Tolerance units, ApproximateEquality a b units) => b -> a -> Expectation
approximately expected actual =
  expect (actual ~= expected) "Given values are not approximately equal"

rangeApproximatelyIncludes :: Tolerance units => Qty units -> Range units -> Expectation
rangeApproximatelyIncludes value givenRange =
  expect (Range.approximatelyIncludes value givenRange) $
    "Given value is not contained in the given range"

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

range :: Tolerance units => Range units -> Range units -> Expectation
range (Range low1 high1) (Range low2 high2) =
  all [approximately low1 low2, approximately high1 high2]

pair :: (a -> a -> Expectation) -> (a, a) -> (a, a) -> Expectation
pair expectation (a1, b1) (a2, b2) = all [expectation a1 a2, expectation b1 b2]
