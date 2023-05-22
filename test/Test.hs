module Test
  ( Test
  , Expectation
  , verify
  , check
  , group
  , expect
  , approximatelyEqual
  , rangeApproximatelyContains
  , run
  )
where

import Console qualified
import List qualified
import OpenSolid
import Random (Generator)
import Random qualified
import Range (Range)
import Range qualified
import Task qualified

data Expectation
  = Passed
  | Failed Text

data Test
  = Test Expectation
  | Fuzz Int (Generator Expectation)
  | Group Text (List Test)

expect :: Bool -> Text -> Expectation
expect True _ = Passed
expect False failureMessage = Failed failureMessage

approximatelyEqual :: (Tolerance units, ApproximateEquality a b units) => a -> b -> Expectation
approximatelyEqual expected actual =
  expect (expected ~= actual) "Given values are not approximately equal"

rangeApproximatelyContains :: Tolerance units => Qty units -> Range units -> Expectation
rangeApproximatelyContains value range =
  expect (Range.contains value (Range.tolerant range)) $
    "Given value is not contained in the given range"

verify :: Expectation -> Test
verify = Test

check :: Int -> Generator Expectation -> Test
check = Fuzz

group :: Text -> List Test -> Test
group = Group

run :: Test -> Task Text ()
run test = do
  success <- runImpl test
  if success
    then Task.succeed ()
    else Task.fail "Failure when running tests"

reportError :: Text -> Task Text Bool
reportError message = do Console.print ("ERROR: " ++ message); Task.succeed False

runImpl :: Test -> Task Text Bool
runImpl (Test Passed) = Task.succeed True
runImpl (Test (Failed message)) = reportError message
runImpl (Group _ tests) = Task.collect runImpl tests |> Task.map (List.all identity)
runImpl (Fuzz count generator) = fuzzImpl count generator

fuzzImpl :: Int -> Generator Expectation -> Task Text Bool
fuzzImpl 0 _ = Task.succeed True
fuzzImpl n generator = do
  expectation <- Random.generate generator
  case expectation of
    Passed -> fuzzImpl (n - 1) generator
    Failed message -> reportError message
