module Script
  ( Script
  , run
  , succeed
  , fail
  , printLine
  , map
  , forEach
  , perform
  )
where

import Control.Exception qualified
import Data.Text.IO qualified
import DoNotation
import OpenSolid
import Result qualified
import System.Exit qualified
import System.IO.Error qualified
import Text qualified
import Prelude qualified

data Script a
  = Done (Result Text a)
  | Perform (IO (Script a))

instance Compose (Script ()) (Script a) (Script a) where
  script1 >> script2 = script1 >>= (\() -> script2)

instance a ~ a' => Bind (Script a') (a' -> Script b) (Script b) where
  Done (Ok value) >>= function = function value
  Done (Error err) >>= _ = Done (Error err)
  Perform io >>= function = Perform (Prelude.fmap (>>= function) io)

instance a ~ a' => Bind (Result x a) (a' -> Script b) (Script b) where
  Ok value >>= function = function value
  Error err >>= _ = Done (Error (errorMessage err))

map :: (a -> b) -> Script a -> Script b
map function (Done result) = Done (Result.map function result)
map function (Perform io) = Perform (Prelude.fmap (map function) io)

perform :: IO a -> Script a
perform io =
  Perform $
    Control.Exception.catch
      (Prelude.fmap succeed io)
      ( \ioError ->
          let message = Text.fromChars (System.IO.Error.ioeGetErrorString ioError)
           in Prelude.pure (fail message)
      )

run :: Script () -> IO ()
run (Done (Ok ())) = System.Exit.exitSuccess
run (Done (Error message)) = run (printLine message) Prelude.>> System.Exit.exitFailure
run (Perform io) = io Prelude.>>= run

succeed :: a -> Script a
succeed value = Done (Ok value)

instance Fail (Script a) where
  fail message = Done (Error message)

printLine :: Text -> Script ()
printLine text = perform (Data.Text.IO.putStrLn text)

forEach :: (a -> Script ()) -> List a -> Script ()
forEach _ [] = succeed ()
forEach function (first : rest) = do
  function first
  forEach function rest
