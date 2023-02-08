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
  )
where

import Control.Exception qualified
import Data.Text.IO qualified
import OpenSolid
import System.Exit qualified
import Text qualified
import Prelude (IOError)
import Prelude qualified

data Script x a
  = Succeed a
  | Error x
  | Perform (IO (Script x a))

map :: (a -> b) -> Script x a -> Script x b
map function (Succeed value) = Succeed (function value)
map _ (Error err) = Error err
map function (Perform io) = Perform (Prelude.fmap (map function) io)

instance Functor (Script x) where
  fmap = map

instance Applicative (Script x) where
  pure = succeed

  Succeed function <*> script = map function script
  Error err <*> _ = Error err
  Perform io <*> script = Perform (Prelude.fmap (Prelude.<*> script) io)

instance Composition (Script x ()) (Script x a) (Script x a) where
  script1 >> script2 = script1 >>= (\() -> script2)

instance Bind (Script x) where
  Succeed value >>= function = function value
  Error err >>= _ = Error err
  Perform io >>= function = Perform (Prelude.fmap (>>= function) io)

instance Prelude.Monad (Script x) where
  (>>=) = (>>=)

perform :: IO a -> Script IOError a
perform io =
  Perform (Control.Exception.catch (Prelude.fmap succeed io) (Prelude.pure Prelude.. error))

type Program = Script IOError ()

run :: Script IOError () -> IO ()
run (Succeed ()) = System.Exit.exitSuccess
run (Error ioError) = Prelude.ioError ioError
run (Perform io) = io Prelude.>>= run

succeed :: a -> Script x a
succeed = Succeed

error :: x -> Script x a
error = Error

fail :: Text -> Script IOError a
fail message = Error (Prelude.userError (Text.toChars message))

printLine :: Text -> Script IOError ()
printLine text = perform (Data.Text.IO.putStrLn text)

forEach :: (a -> Script x ()) -> List a -> Script x ()
forEach _ [] = succeed ()
forEach function (first : rest) = do
  function first
  forEach function rest
