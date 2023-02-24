module Script
  ( Script
  , IOError
  , Program
  , run
  , succeed
  , error
  , fail
  , printLine
  , map
  , forEach
  , perform
  , (>>=)
  , (>>)
  )
where

import Control.Exception qualified
import Data.Text.IO qualified
import OpenSolid
import Result qualified
import System.Exit qualified
import Text qualified
import Prelude (IOError)
import Prelude qualified

data Script x a
  = Done (Result x a)
  | Perform (IO (Script x a))

map :: (a -> b) -> Script x a -> Script x b
map function (Done result) = Done (Result.map function result)
map function (Perform io) = Perform (Prelude.fmap (map function) io)

instance Prelude.Functor (Script x) where
  fmap = map

instance Prelude.Applicative (Script x) where
  pure = succeed

  Done (Ok function) <*> script = map function script
  Done (Error err) <*> _ = Done (Error err)
  Perform io <*> script = Perform (Prelude.fmap (Prelude.<*> script) io)

instance Composition (Script x ()) (Script x a) (Script x a) where
  script1 >> script2 = script1 >>= (\() -> script2)

(>>=) :: Script x a -> (a -> Script x b) -> Script x b
Done (Ok value) >>= function = function value
Done (Error err) >>= _ = Done (Error err)
Perform io >>= function = Perform (Prelude.fmap (>>= function) io)

perform :: IO a -> Script IOError a
perform io =
  Perform (Control.Exception.catch (Prelude.fmap succeed io) (Prelude.pure Prelude.. error))

type Program = Script IOError ()

run :: Script IOError () -> IO ()
run (Done (Ok ())) = System.Exit.exitSuccess
run (Done (Error ioError)) = Prelude.ioError ioError
run (Perform io) = io Prelude.>>= run

succeed :: a -> Script x a
succeed value = Done (Ok value)

error :: x -> Script x a
error err = Done (Error err)

fail :: Text -> Script IOError a
fail message = Done (Error (Prelude.userError (Text.toChars message)))

printLine :: Text -> Script IOError ()
printLine text = perform (Data.Text.IO.putStrLn text)

forEach :: (a -> Script x ()) -> List a -> Script x ()
forEach _ [] = succeed ()
forEach function (first : rest) = do
  function first
  forEach function rest
