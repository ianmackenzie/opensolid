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
import Text qualified

data Expectation
  = Passed
  | Failed Text

data Test
  = Verify Text Expectation
  | Check Int Text (Generator Expectation)
  | Group Text (List Test)

expect :: Bool -> Text -> Expectation
expect True _ = Passed
expect False failureMessage = Failed failureMessage

approximatelyEqual :: (Tolerance units, ApproximateEquality a b units) => a -> b -> Expectation
approximatelyEqual expected actual =
  expect (expected ~= actual) "Given values are not approximately equal"

rangeApproximatelyContains :: Tolerance units => Qty units -> Range units -> Expectation
rangeApproximatelyContains value range =
  expect (Range.approximatelyIncludes value range) "Given value is not contained in the given range"

verify :: Text -> Expectation -> Test
verify = Verify

check :: Int -> Text -> Generator Expectation -> Test
check = Check

group :: Text -> List Test -> Test
group = Group

run :: List Test -> Task Text ()
run tests = do
  Console.printLine ""
  Console.printLine "Running tests..."
  Console.printLine ""
  results <- Task.collect (runImpl []) tests
  if List.all identity results
    then do
      Console.printLine "✅ All tests passed"
      Task.succeed ()
    else Task.fail "Failure when running tests"

reportError :: List Text -> Text -> Task Text Bool
reportError context message = do
  Console.printLine ("❌ " ++ (Text.join " | " (List.reverse context) ++ ":"))
  Console.printLine ""
  Console.printLine (Text.indent "   " message)
  Console.printLine ""
  Task.succeed False

runImpl :: List Text -> Test -> Task Text Bool
runImpl _ (Verify _ Passed) = Task.succeed True
runImpl context (Verify label (Failed message)) = reportError (label : context) message
runImpl context (Group label tests) = Task.collect (runImpl (label : context)) tests |> Task.map (List.all identity)
runImpl context (Check count label generator) = fuzzImpl (label : context) count generator

fuzzImpl :: List Text -> Int -> Generator Expectation -> Task Text Bool
fuzzImpl _ 0 _ = Task.succeed True
fuzzImpl context n generator = do
  expectation <- Random.generate generator
  case expectation of
    Passed -> fuzzImpl context (n - 1) generator
    Failed message -> reportError context message
