module Test
  ( Test
  , Expectation
  , verify
  , check
  , group
  , run
  , expect
  , Test.show
  , lines
  )
where

import Console qualified
import Data.Foldable qualified
import Debug qualified
import List qualified
import OpenSolid
import Random (Generator)
import Random qualified
import Task qualified
import Text qualified
import Prelude (show)

data Expectation
  = Passed
  | Failed Text

instance (ErrorMessage x, a ~ a') => Bind (Result x a) a' Expectation where
  bind f (Ok value) = f value
  bind _ (Error error) = Failed (errorMessage error)

instance (ErrorMessage x, a ~ a') => Bind (Result x a) a' (Generator Expectation) where
  bind f (Ok value) = f value
  bind _ (Error error) = Random.return (Failed (errorMessage error))

data Test
  = Check Int Text (Generator Expectation)
  | Group Text (List Test)

verify :: Text -> Generator Expectation -> Test
verify = check 1

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

expect :: Bool -> List Text -> Generator Expectation
expect True _ = Random.return Passed
expect False failureOutput = Random.return (Failed (Text.join "\n" failureOutput))

show :: Show a => Text -> a -> Text
show label value = label ++ ": " ++ Debug.show value

newtype Lines a = Lines (List a)

instance Show a => Show (Lines a) where
  show (Lines values) =
    List.map (("\n  " ++) << Debug.show) values
      |> Text.concat
      |> Text.toChars

lines :: (Data.Foldable.Foldable container, Show a) => container a -> Lines a
lines container = Lines (Data.Foldable.toList container)
