module Test
  ( Test
  , Expectation
  , verify
  , check
  , group
  , run
  , expect
  , expectAll
  , output
  , lines
  , pass
  , fail
  , (>>=)
  )
where

import Data.Foldable qualified
import Error qualified
import IO qualified
import List qualified
import OpenSolid
import Random (Generator)
import Random qualified
import Result qualified
import String qualified

data Expectation
  = Passed
  | Failed (List String)

class Bind a b c where
  (>>=) :: a -> (b -> c) -> c

instance a ~ a' => Bind (Generator a) a' (Generator b) where
  (>>=) = (Random.>>=)

instance a ~ a' => Bind (Result x a) a' (Result x b) where
  (>>=) = (Result.>>=)

instance a ~ a' => Bind (IO a) a' (IO b) where
  (>>=) = (IO.>>=)

instance a ~ a' => Bind (Result x a) a' Expectation where
  Ok value >>= f = f value
  Error error >>= _ = Failed [Error.message error]

instance a ~ a' => Bind (Result x a) a' (Generator Expectation) where
  Ok value >>= f = f value
  Error error >>= _ = Random.return (Failed [Error.message error])

data Test
  = Check Int String (Generator Expectation)
  | Group String (List Test)

verify :: String -> Generator Expectation -> Test
verify = check 1

check :: Int -> String -> Generator Expectation -> Test
check = Check

group :: String -> List Test -> Test
group = Group

testCount :: Int -> String -> String
testCount count description =
  let pluralized = if count == 1 then "test" else "tests"
   in String.join " " [String.fromInt count, pluralized, description]

run :: List Test -> IO ()
run tests = IO.do
  IO.printLine ""
  IO.printLine "Running tests..."
  IO.printLine ""
  results <- IO.collect (runImpl []) tests
  let (successes, failures) = sum results
  if failures == 0
    then IO.printLine ("✅ " ++ testCount successes "passed")
    else IO.fail (testCount failures "failed")

reportError :: List String -> List String -> IO (Int, Int)
reportError context messages = IO.do
  IO.printLine ("❌ " ++ (String.join " | " (List.reverse context) ++ ":"))
  IO.printLine ""
  IO.forEach messages (String.indent "   " >> IO.printLine)
  IO.printLine ""
  IO.return (0, 1)

runImpl :: List String -> Test -> IO (Int, Int)
runImpl context (Check count label generator) = fuzzImpl (label : context) count generator
runImpl context (Group label tests) = IO.collect (runImpl (label : context)) tests |> IO.map sum

sum :: List (Int, Int) -> (Int, Int)
sum [] = (0, 0)
sum ((successes, failures) : rest) =
  let (restSuccesses, restFailures) = sum rest
   in (successes + restSuccesses, failures + restFailures)

fuzzImpl :: List String -> Int -> Generator Expectation -> IO (Int, Int)
fuzzImpl _ 0 _ = IO.return (1, 0)
fuzzImpl context n generator = IO.do
  expectation <- Random.generate generator
  case expectation of
    Passed -> fuzzImpl context (n - 1) generator
    Failed messages -> reportError context messages

pass :: Generator Expectation
pass = Random.return Passed

fail :: String -> Generator Expectation
fail message = Random.return (Failed [message])

expect :: Bool -> Generator Expectation
expect True = Random.return Passed
expect False = Random.return (Failed [])

expectAll :: List Bool -> Generator Expectation
expectAll checks = expect (List.allTrue checks)

output :: Show a => String -> a -> Generator Expectation -> Generator Expectation
output label value =
  Random.map <|
    \case
      Passed -> Passed
      Failed messages -> Failed (messages ++ [label ++ ": " ++ show value])

newtype Lines a = Lines (List a)

instance Show a => Show (Lines a) where
  show (Lines values) = String.concat (List.map (\value -> "\n  " ++ show value) values)

lines :: (Data.Foldable.Foldable container, Show a) => container a -> Lines a
lines container = Lines (Data.Foldable.toList container)
