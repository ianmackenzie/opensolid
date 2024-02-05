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

import Console qualified
import Data.Foldable qualified
import List qualified
import OpenSolid hiding (fail, (>>=))
import OpenSolid qualified
import Random (Generator)
import Random qualified
import String qualified
import Task qualified

data Expectation
  = Passed
  | Failed (List String)

class Bind a b c where
  bind :: (b -> c) -> a -> c

(>>=) :: Bind a b c => a -> (b -> c) -> c
a >>= f = bind f a

instance a ~ a' => Bind (Generator a) a' (Generator b) where
  bind function generator = generator OpenSolid.>>= function

instance a ~ a' => Bind (Result x a) a' (Result x b) where
  bind function result = result OpenSolid.>>= function

instance a ~ a' => Bind (Task x a) a' (Task x b) where
  bind function task = task OpenSolid.>>= function

instance a ~ a' => Bind (Result x a) a' Expectation where
  bind f (Ok value) = f value
  bind _ (Error error) = Failed [errorMessage error]

instance a ~ a' => Bind (Result x a) a' (Generator Expectation) where
  bind f (Ok value) = f value
  bind _ (Error error) = Random.return (Failed [errorMessage error])

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

run :: List Test -> Task String ()
run tests = do
  Console.printLine ""
  Console.printLine "Running tests..."
  Console.printLine ""
  results <- Task.collect (runImpl []) tests
  let (successes, failures) = sum results
  if failures == 0
    then do
      Console.printLine ("✅ " ++ testCount successes "passed")
      return ()
    else Task.fail (testCount failures "failed")

reportError :: List String -> List String -> Task String (Int, Int)
reportError context messages = do
  Console.printLine ("❌ " ++ (String.join " | " (List.reverse context) ++ ":"))
  Console.printLine ""
  Task.forEach messages (Console.printLine . String.indent "   ")
  Console.printLine ""
  return (0, 1)

runImpl :: List String -> Test -> Task String (Int, Int)
runImpl context (Check count label generator) = fuzzImpl (label : context) count generator
runImpl context (Group label tests) = Task.collect (runImpl (label : context)) tests |> Task.map sum

sum :: List (Int, Int) -> (Int, Int)
sum [] = (0, 0)
sum ((successes, failures) : rest) =
  let (restSuccesses, restFailures) = sum rest
   in (successes + restSuccesses, failures + restFailures)

fuzzImpl :: List String -> Int -> Generator Expectation -> Task String (Int, Int)
fuzzImpl _ 0 _ = return (1, 0)
fuzzImpl context n generator = do
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
  show (Lines values) = String.concat (List.map (("\n  " ++) . show) values)

lines :: (Data.Foldable.Foldable container, Show a) => container a -> Lines a
lines container = Lines (Data.Foldable.toList container)
