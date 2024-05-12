module Test
  ( Test
  , Expectation
  , verify
  , check
  , group
  , run
  , expect
  , all
  , output
  , lines
  , pass
  , fail
  , (>>=)
  , (>>)
  )
where

import Data.Foldable qualified
import Error qualified
import IO qualified
import List qualified
import OpenSolid
import Random (Generator)
import Random qualified
import String qualified

data TestResult
  = Passed
  | Failed (List String)

newtype Expectation = Expectation (Generator TestResult)

unwrap :: Expectation -> Generator TestResult
unwrap (Expectation generator) = generator

class Bind m where
  (>>=) :: m a -> (a -> Expectation) -> Expectation

instance Bind Generator where
  valueGenerator >>= toExpectation =
    Expectation Random.do
      value <- valueGenerator
      unwrap (toExpectation value)

instance Bind (Result x) where
  Ok value >>= f = f value
  Error error >>= _ = Expectation (Random.return (Failed [Error.message error]))

data Test
  = Check Int String Expectation
  | Group String (List Test)

verify :: String -> Expectation -> Test
verify = check 1

check :: Int -> String -> Expectation -> Test
check = Check

group :: String -> List Test -> Test
group = Group

testCount :: Int -> String -> String
testCount count description = do
  let pluralized = if count == 1 then "test" else "tests"
  String.join " " [String.fromInt count, pluralized, description]

run :: List Test -> IO ()
run tests = IO.do
  IO.printLine ""
  IO.printLine "Running tests..."
  IO.printLine ""
  results <- IO.collect (runImpl []) tests
  let (successes, failures) = sum results
  if failures == 0
    then IO.printLine ("✅ " + testCount successes "passed")
    else IO.fail (testCount failures "failed")

reportError :: List String -> List String -> IO (Int, Int)
reportError context messages = IO.do
  IO.printLine ("❌ " + (String.join " | " (List.reverse context) + ":"))
  IO.printLine ""
  IO.forEach messages (String.indent "   " >> IO.printLine)
  IO.printLine ""
  IO.return (0, 1)

runImpl :: List String -> Test -> IO (Int, Int)
runImpl context (Check count label generator) = fuzzImpl (label : context) count generator
runImpl context (Group label tests) = IO.collect (runImpl (label : context)) tests |> IO.map sum

sum :: List (Int, Int) -> (Int, Int)
sum [] = (0, 0)
sum ((successes, failures) : rest) = do
  let (restSuccesses, restFailures) = sum rest
  (successes + restSuccesses, failures + restFailures)

fuzzImpl :: List String -> Int -> Expectation -> IO (Int, Int)
fuzzImpl _ 0 _ = IO.return (1, 0)
fuzzImpl context n expectation = IO.do
  let (Expectation generator) = expectation
  testResult <- Random.generate generator
  case testResult of
    Passed -> fuzzImpl context (n - 1) expectation
    Failed messages -> reportError context messages

pass :: Expectation
pass = Expectation (Random.return Passed)

fail :: String -> Expectation
fail message = Expectation (Random.return (Failed [message]))

expect :: Bool -> Expectation
expect True = pass
expect False = Expectation (Random.return (Failed []))

combineTestResults :: List TestResult -> TestResult
combineTestResults [] = Passed
combineTestResults (Passed : rest) = combineTestResults rest
combineTestResults (Failed messages : rest) = case combineTestResults rest of
  Passed -> Failed messages
  Failed restMessages -> Failed (messages + restMessages)

all :: List Expectation -> Expectation
all expectations =
  Expectation (Random.map combineTestResults (Random.combine (List.map unwrap expectations)))

output :: Show a => String -> a -> Expectation -> Expectation
output label value (Expectation generator) =
  Expectation $ Random.map (addOutput label value) generator

addOutput :: Show a => String -> a -> TestResult -> TestResult
addOutput _ _ Passed = Passed
addOutput label value (Failed messages) = Failed (messages + [label + ": " + show value])

newtype Lines a = Lines (List a)

instance Show a => Show (Lines a) where
  show (Lines values) = String.concat (List.map (\value -> "\n  " + show value) values)

lines :: (Data.Foldable.Foldable container, Show a) => container a -> Lines a
lines container = Lines (Data.Foldable.toList container)
