module Test
  ( Test
  , Expectation
  , verify
  , check
  , group
  , run
  , assert
  , expect
  )
where

import Console qualified
import Expect (Expectation (Failed, Passed))
import List qualified
import OpenSolid
import Random (Generator)
import Random qualified
import Task qualified
import Text qualified

data Test
  = Verify Text Expectation
  | Check Int Text (Generator Expectation)
  | Group Text (List Test)

verify :: Text -> Expectation -> Test
verify = Verify

check :: Int -> Text -> Generator Expectation -> Test
check = Check

group :: Text -> List Test -> Test
group = Group

testCount :: Int -> Text -> Text
testCount count description =
  let pluralized = if count == 1 then "test" else "tests"
   in Text.join " " [Text.fromInt count, pluralized, description]

run :: List Test -> Task Text ()
run tests = do
  Console.printLine ""
  Console.printLine "Running tests..."
  Console.printLine ""
  results <- Task.collect (runImpl []) tests
  let (successes, failures) = sum results
  if failures == 0
    then do
      Console.printLine ("✅ " ++ testCount successes "passed")
      Task.succeed ()
    else Task.fail (testCount failures "failed")

reportError :: List Text -> Text -> Task Text (Int, Int)
reportError context message = do
  Console.printLine ("❌ " ++ (Text.join " | " (List.reverse context) ++ ":"))
  Console.printLine ""
  Console.printLine (Text.indent "   " message)
  Console.printLine ""
  Task.succeed (0, 1)

runImpl :: List Text -> Test -> Task Text (Int, Int)
runImpl _ (Verify _ Passed) = Task.succeed (1, 0)
runImpl context (Verify label (Failed message)) = reportError (label : context) message
runImpl context (Check count label generator) = fuzzImpl (label : context) count generator
runImpl context (Group label tests) = Task.collect (runImpl (label : context)) tests |> Task.map sum

sum :: List (Int, Int) -> (Int, Int)
sum [] = (0, 0)
sum ((successes, failures) : rest) =
  let (restSuccesses, restFailures) = sum rest
   in (successes + restSuccesses, failures + restFailures)

fuzzImpl :: List Text -> Int -> Generator Expectation -> Task Text (Int, Int)
fuzzImpl _ 0 _ = Task.succeed (1, 0)
fuzzImpl context n generator = do
  expectation <- Random.generate generator
  case expectation of
    Passed -> fuzzImpl context (n - 1) generator
    Failed message -> reportError context message

assert :: Bool -> List Text -> Expectation
assert True _ = Passed
assert False failureOutput = Failed (Text.paragraph failureOutput)

expect :: Bool -> List Text -> Generator Expectation
expect passed failureOutput = Random.return (assert passed failureOutput)
