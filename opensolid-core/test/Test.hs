module Test
  ( Test
  , Expectation
  , Generator
  , generate
  , abort
  , verify
  , check
  , group
  , run
  , expect
  , all
  , combine
  , output
  , lines
  , pass
  , fail
  )
where

import Control.Exception (SomeException)
import Control.Exception qualified
import OpenSolid.Duration qualified as Duration
import OpenSolid.IO qualified as IO
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude hiding (fail)
import OpenSolid.Random qualified as Random
import OpenSolid.Text qualified as Text
import OpenSolid.Timer qualified as Timer
import OpenSolid.Tolerance qualified as Tolerance
import System.Console.ANSI qualified as AnsiTerminal
import System.Environment
import System.Exit qualified
import Text.Printf qualified
import Prelude qualified

data TestResult a = Succeeded a | Failed (List Text)

instance Functor TestResult where
  fmap function (Succeeded value) = Succeeded (function value)
  fmap _ (Failed messages) = Failed messages

instance Applicative TestResult where
  pure = Succeeded

  Succeeded function <*> Succeeded value = Succeeded (function value)
  Failed messages <*> _ = Failed messages
  Succeeded _ <*> Failed messages = Failed messages

newtype Generator a = Generator (Random.Generator (TestResult a))

unwrap :: Generator a -> Random.Generator (TestResult a)
unwrap (Generator generator) = generator

instance Functor Generator where
  fmap function (Generator generator) = Generator (Random.map (Prelude.fmap function) generator)

instance Applicative Generator where
  pure value = Generator (Random.return (Succeeded value))
  Generator functionGenerator <*> Generator valueGenerator = Generator do
    functionResult <- functionGenerator
    valueResult <- valueGenerator
    Random.return (functionResult <*> valueResult)

type Expectation = Generator ()

instance Monad Generator where
  Generator generator >>= function = Generator do
    result <- generator
    case result of
      Succeeded value -> unwrap (function value)
      Failed errors -> Random.return (Failed errors)

instance MonadFail Generator where
  fail message = Generator (Random.return (Failed [Text.pack message]))

generate :: Random.Generator a -> Generator a
generate generator = Generator (Random.map Succeeded generator)

data Test
  = Abort Text
  | Check Int Text ~Expectation
  | Group Text (List Test)

abort :: Text -> Test
abort = Abort

verify :: Text -> (Tolerance Meters => Expectation) -> Test
verify = check 1

check :: Int -> Text -> (Tolerance Meters => Expectation) -> Test
check count label expectation =
  Check count label (Tolerance.using Length.defaultTolerance expectation)

group :: Text -> List Test -> Test
group = Group

successColor :: AnsiTerminal.Color
successColor = AnsiTerminal.Green

errorColor :: AnsiTerminal.Color
errorColor = AnsiTerminal.Red

withTextColor :: AnsiTerminal.Color -> IO a -> IO a
withTextColor color io = do
  let setColorSGR = AnsiTerminal.SetColor AnsiTerminal.Foreground AnsiTerminal.Vivid color
  let setColor = AnsiTerminal.setSGR [setColorSGR]
  let resetColor = AnsiTerminal.setSGR [AnsiTerminal.Reset]
  IO.bracket setColor (const resetColor) (const io)

run :: List Test -> IO ()
run tests = do
  IO.printLine "Running tests..."
  argStrings <- System.Environment.getArgs
  let args = List.map Text.pack argStrings
  results <- IO.collect (runImpl args "") tests
  let (successes, failures) = sum results
  let printSummary color count description =
        withTextColor color $
          IO.printLine $
            Text.sentence
              [ Text.int count
              , Text.pluralize "test" "tests" count
              , description
              ]
  if failures == 0
    then do
      printSummary successColor successes "passed"
      System.Exit.exitSuccess
    else do
      printSummary errorColor failures "failed"
      System.Exit.exitFailure

reportError :: Text -> List Text -> IO (Int, Int)
reportError context messages = do
  withTextColor errorColor (IO.printLine (context <> " failed:"))
  IO.forEach messages (Text.indent "   " >> IO.printLine)
  IO.succeed (0, 1)

runImpl :: List Text -> Text -> Test -> IO (Int, Int)
runImpl args context test = case test of
  Abort message -> reportError context [message]
  Check count label generator -> do
    let fullName = appendTo context label
    let runCheck =
          Control.Exception.catch
            (fuzzImpl fullName count (Random.init 0) generator)
            (\(exception :: SomeException) -> reportError fullName [Text.show exception])
    if
      | List.isEmpty args ->
          -- No test filter specified, silently run all tests
          runCheck
      | List.any (\arg -> Text.contains arg fullName) args -> do
          -- Test filter specified, so print out which tests we're running
          -- and how long they took (with an extra emoji to flag slow tests)
          timer <- Timer.start
          results <- runCheck
          elapsed <- Timer.elapsed timer
          let elapsedText = fixed 3 (Duration.inSeconds elapsed) <> "s"
          let elapsedSuffix = if elapsed > Duration.seconds 0.1 then " ⏲️" else ""
          IO.printLine (fullName <> ": " <> elapsedText <> elapsedSuffix)
          IO.succeed results
      | otherwise ->
          -- Current test didn't match filter, so return 0 successes and 0 failures
          IO.succeed (0, 0)
  Group label tests -> do
    successesAndFailuresPerGroup <- IO.collect (runImpl args (appendTo context label)) tests
    IO.succeed (sum successesAndFailuresPerGroup)

fixed :: Int -> Number -> Text
fixed decimalPlaces value = do
  let formatString = "%." <> Text.int decimalPlaces <> "f"
  Text.pack (Text.Printf.printf (Text.unpack formatString) (Number.toDouble value))

appendTo :: Text -> Text -> Text
appendTo "" name = name
appendTo context name = context <> "." <> name

sum :: List (Int, Int) -> (Int, Int)
sum [] = (0, 0)
sum ((successes, failures) : rest) = do
  let (restSuccesses, restFailures) = sum rest
  (successes + restSuccesses, failures + restFailures)

fuzzImpl :: Text -> Int -> Random.Seed -> Expectation -> IO (Int, Int)
fuzzImpl context n seed expectation = case n of
  0 -> IO.succeed (1, 0) -- We've finished fuzzing, report 1 successful test
  _ -> do
    let Generator generator = expectation
    let (testResult, updatedSeed) = Random.step generator seed
    case testResult of
      Succeeded () -> fuzzImpl context (n - 1) updatedSeed expectation
      Failed messages -> reportError context messages

pass :: Expectation
pass = Generator (Random.return (Succeeded ()))

fail :: Text -> Expectation
fail message = Generator (Random.return (Failed [message]))

expect :: Bool -> Expectation
expect True = pass
expect False = Generator (Random.return (Failed []))

combineTestResults :: List (TestResult ()) -> TestResult ()
combineTestResults [] = Succeeded ()
combineTestResults (Succeeded () : rest) = combineTestResults rest
combineTestResults (Failed messages : rest) = case combineTestResults rest of
  Succeeded () -> Failed messages
  Failed restMessages -> Failed (messages <> restMessages)

all :: List Expectation -> Expectation
all expectations =
  Generator (Random.map combineTestResults (Random.collect unwrap expectations))

combine :: (a -> Expectation) -> List a -> Expectation
combine function values = all (List.map function values)

output :: Show a => Text -> a -> Expectation -> Expectation
output label value (Generator generator) =
  Generator (Random.map (addOutput label value) generator)

addOutput :: Show a => Text -> a -> TestResult () -> TestResult ()
addOutput _ _ (Succeeded ()) = Succeeded ()
addOutput label value (Failed messages) = Failed (messages <> [label <> ": " <> Text.show value])

newtype Lines a = Lines (List a)

instance Show a => Show (Lines a) where
  show (Lines values) =
    Text.unpack $
      Text.concat (List.map (\value -> "\n  " <> Text.show value) values)

lines :: List a -> Lines a
lines = Lines
