module Script
  ( Script
  , IOError
  , Program
  , run
  , succeed
  , error
  , fail
  , printLine
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

instance Functor (Script x) where
  fmap function (Succeed value) = Succeed (function value)
  fmap _ (Error err) = Error err
  fmap function (Perform io) = Perform (fmap (fmap function) io)

instance Applicative (Script x) where
  pure = succeed

  Succeed function <*> script = fmap function script
  Error err <*> _ = Error err
  Perform io <*> script = Perform (fmap (<*> script) io)

instance Monad (Script x) where
  Succeed value >>= function = function value
  Error err >>= _ = Error err
  Perform io >>= function = Perform (fmap (>>= function) io)

perform :: IO a -> Script IOError a
perform io = Perform (Control.Exception.catch (fmap succeed io) (pure <<< error))

type Program = Script IOError ()

run :: Script IOError () -> IO ()
run (Succeed ()) = System.Exit.exitSuccess
run (Error ioError) = Prelude.ioError ioError
run (Perform io) = io >>= run

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
