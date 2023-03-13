module Task
  ( Task
  , run
  , succeed
  , fail
  , printLine
  , map
  , mapError
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

data Task x a
  = Done (Result x a)
  | Perform (IO (Task x a))

instance x ~ x' => Compose (Task x ()) (Task x' a) (Task x a) where
  script1 >> script2 = script1 >>= (\() -> script2)

instance x ~ x' => Bind (Task x) (Task x' b) where
  Done (Ok value) >>= function = function value
  Done (Error err) >>= _ = Done (Error err)
  Perform io >>= function = Perform (Prelude.fmap (>>= function) io)

instance x ~ x' => Bind (Result x) (Task x' b) where
  Ok value >>= function = function value
  Error err >>= _ = Done (Error err)

instance Bind [] (Task x ()) where
  [] >>= _ = succeed ()
  (first : rest) >>= function = function first >> (rest >>= function)

map :: (a -> b) -> Task x a -> Task x b
map function (Done result) = Done (Result.map function result)
map function (Perform io) = Perform (Prelude.fmap (map function) io)

mapError :: IsError y => (x -> y) -> Task x a -> Task y a
mapError function (Done result) = Done (Result.mapError function result)
mapError function (Perform io) = Perform (Prelude.fmap (mapError function) io)

perform :: IO a -> Task Text a
perform io =
  Perform $
    Control.Exception.catch
      (Prelude.fmap succeed io)
      ( \ioError ->
          let message = Text.fromChars (System.IO.Error.ioeGetErrorString ioError)
           in Prelude.pure (fail message)
      )

run :: Task x () -> IO ()
run (Done (Ok ())) = System.Exit.exitSuccess
run (Done (Error err)) = run (printLine (errorMessage err)) Prelude.>> System.Exit.exitFailure
run (Perform io) = io Prelude.>>= run

succeed :: a -> Task x a
succeed value = Done (Ok value)

instance Fail (Task Text a) where
  fail message = Done (Error message)

printLine :: Text -> Task Text ()
printLine text = perform (Data.Text.IO.putStrLn text)

forEach :: (a -> Task x ()) -> List a -> Task x ()
forEach function list = do item <- list; function item
