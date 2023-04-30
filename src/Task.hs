module Task
  ( Task
  , toIO
  , succeed
  , fail
  , map
  , mapError
  , forEach
  , fromIO
  )
where

import Basics
import Concatenate
import Control.Exception qualified
import DoNotation
import Result (IsError (errorMessage), Result (Error, Ok))
import Result qualified
import System.Exit qualified
import Text qualified
import Prelude qualified

data Task x a
  = Done (Result x a)
  | Perform (IO (Task x a))

instance x ~ x' => Compose (Task x ()) (Task x' a) (Task x a) where
  compose script1 script2 = bind (always script2) script1

instance x ~ x' => Bind (Task x) (Task x' b) where
  bind f (Done (Ok value)) = f value
  bind _ (Done (Error err)) = Done (Error err)
  bind f (Perform io) = Perform (Prelude.fmap (bind f) io)

instance x ~ x' => Bind (Result x) (Task x' b) where
  bind f (Ok value) = f value
  bind _ (Error err) = Done (Error err)

instance Bind [] (Task x ()) where
  bind _ [] = succeed ()
  bind f (first : rest) = compose (f first) (bind f rest)

instance Bind [] (Task x (List b)) where
  bind _ [] = succeed []
  bind f (first : rest) =
    bind (\firstResults -> Task.map (firstResults ++) (bind f rest)) (f first)

instance Prelude.Functor (Task x) where
  fmap f (Done result) = Done (Result.map f result)
  fmap f (Perform io) = Perform (Prelude.fmap (Task.map f) io)

instance Prelude.Applicative (Task x) where
  pure = Ok >> Done
  Done (Ok function) <*> task = Task.map function task
  Done (Error error) <*> _ = Done (Error error)
  Perform io <*> task = Perform (Prelude.fmap (Prelude.<*> task) io)

instance Prelude.Monad (Task x) where
  Done (Ok value) >>= function = function value
  Done (Error error) >>= _ = Done (Error error)
  Perform io >>= function = Perform (Prelude.fmap (Prelude.>>= function) io)

instance Prelude.MonadFail (Task (List Char)) where
  fail = Error >> Done

map :: (a -> b) -> Task x a -> Task x b
map function (Done result) = Done (Result.map function result)
map function (Perform io) = Perform (Prelude.fmap (map function) io)

mapError :: IsError y => (x -> y) -> Task x a -> Task y a
mapError function (Done result) = Done (Result.mapError function result)
mapError function (Perform io) = Perform (Prelude.fmap (mapError function) io)

fromIO :: IO a -> Task IOError a
fromIO io =
  Perform $
    Control.Exception.catch (Prelude.fmap succeed io) $
      (\ioError -> Prelude.pure (Done (Error ioError)))

toIO :: Task x () -> IO ()
toIO (Done (Ok ())) = System.Exit.exitSuccess
toIO (Done (Error error)) = System.Exit.die (Text.toChars (errorMessage error))
toIO (Perform io) = io Prelude.>>= toIO

succeed :: a -> Task x a
succeed value = Done (Ok value)

instance Fail (Task Text a) where
  fail message = Done (Error message)

forEach :: (a -> Task x ()) -> List a -> Task x ()
forEach function list = do item <- list; function item
