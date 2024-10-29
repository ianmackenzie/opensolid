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
import Duration qualified
import Error qualified
import Float qualified
import IO qualified
import List qualified
import OpenSolid
import Random (Generator)
import Random qualified
import System.Console.ANSI qualified
import System.Environment
import Text qualified
import Text.Printf qualified
import Prelude qualified

data TestResult
  = Passed
  | Failed (List Text)

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
  Success value >>= f = f value
  Failure error >>= _ = fail error

data Test
  = Check Int Text ~Expectation
  | Group Text (List Test)

verify :: Text -> Expectation -> Test
verify = check 1

check :: Int -> Text -> Expectation -> Test
check = Check

group :: Text -> List Test -> Test
group = Group

testCount :: Int -> Text -> Text
testCount count description = do
  let pluralized = if count == 1 then "test" else "tests"
  Text.join " " [Text.int count, pluralized, description]

run :: List Test -> IO ()
run tests = IO.do
  IO.printLine "Running tests..."
  argStrings <- System.Environment.getArgs
  let args = List.map Text.pack argStrings
  results <- IO.collect (runImpl args "") tests
  let (successes, failures) = sum results
  if failures == 0
    then IO.do
      System.Console.ANSI.setSGR [System.Console.ANSI.SetColor System.Console.ANSI.Foreground System.Console.ANSI.Vivid System.Console.ANSI.Green]
      IO.printLine (testCount successes "passed")
      System.Console.ANSI.setSGR [System.Console.ANSI.Reset]
    else IO.fail (testCount failures "failed")

reportError :: Text -> List Text -> IO (Int, Int)
reportError context messages = IO.do
  System.Console.ANSI.setSGR [System.Console.ANSI.SetColor System.Console.ANSI.Foreground System.Console.ANSI.Vivid System.Console.ANSI.Red]
  IO.printLine (context + " failed:")
  System.Console.ANSI.setSGR [System.Console.ANSI.Reset]
  IO.forEach messages (Text.indent "   " >> IO.printLine)
  IO.succeed (0, 1)

runImpl :: List Text -> Text -> Test -> IO (Int, Int)
runImpl args context test = case test of
  Check count label generator -> do
    let fullName = appendTo context label
    let initialSeed = Random.init 0
    if
      | List.isEmpty args ->
          -- No test filter specified, silently run all tests
          fuzzImpl fullName count initialSeed generator
      | List.anySatisfy (\arg -> Text.contains arg fullName) args -> IO.do
          -- Test filter specified, so print out which tests we're running
          -- and how long they took (with an extra emoji to flag slow tests)
          (results, elapsed) <- IO.time (fuzzImpl fullName count initialSeed generator)
          let elapsedText = fixed 3 (Duration.inSeconds elapsed) + "s"
          let elapsedSuffix = if elapsed > Duration.seconds 0.1 then " ⏲️" else ""
          IO.printLine (fullName + ": " + elapsedText + elapsedSuffix)
          IO.succeed results
      | otherwise ->
          -- Current test didn't match filter, so return 0 successes and 0 failures
          IO.succeed (0, 0)
  Group label tests -> IO.do
    successesAndFailuresPerGroup <- IO.collect (runImpl args (appendTo context label)) tests
    IO.succeed (sum successesAndFailuresPerGroup)

fixed :: Int -> Float -> Text
fixed decimalPlaces value = do
  let formatString = "%." + Text.int decimalPlaces + "f"
  Text.pack (Text.Printf.printf (Text.unpack formatString) (Float.toDouble value))

appendTo :: Text -> Text -> Text
appendTo "" name = name
appendTo context name = context + "." + name

sum :: List (Int, Int) -> (Int, Int)
sum [] = (0, 0)
sum ((successes, failures) : rest) = do
  let (restSuccesses, restFailures) = sum rest
  (successes + restSuccesses, failures + restFailures)

fuzzImpl :: Text -> Int -> Random.Seed -> Expectation -> IO (Int, Int)
fuzzImpl context n seed expectation = case n of
  0 -> IO.succeed (1, 0) -- We've finished fuzzing, report 1 successful test
  _ -> IO.do
    let (Expectation generator) = expectation
    let (testResult, updatedSeed) = Random.step generator seed
    case testResult of
      Passed -> fuzzImpl context (n - 1) updatedSeed expectation
      Failed messages -> reportError context messages

pass :: Expectation
pass = Expectation (Random.return Passed)

fail :: Error.Message x => x -> Expectation
fail error = Expectation (Random.return (Failed [Error.message error]))

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

output :: Show a => Text -> a -> Expectation -> Expectation
output label value (Expectation generator) =
  Expectation $ Random.map (addOutput label value) generator

addOutput :: Show a => Text -> a -> TestResult -> TestResult
addOutput _ _ Passed = Passed
addOutput label value (Failed messages) = Failed (messages + [label + ": " + Text.show value])

newtype Lines a = Lines (List a)

instance Show a => Show (Lines a) where
  show (Lines values) =
    Text.unpack $
      Text.concat (List.map (\value -> "\n  " + Text.show value) values)

lines :: (Data.Foldable.Foldable container, Show a) => container a -> Lines a
lines container = Lines (Data.Foldable.toList container)
