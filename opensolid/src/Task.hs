module Task
  ( Task
  , evaluate
  , main
  , fail
  , map
  , mapError
  , forEach
  , fromIO
  , toIO
  , collect
  )
where

import Basics
import Control.Exception qualified
import Result (ErrorMessage (errorMessage), Result (Error, Ok))
import Result qualified
import System.Exit qualified
import Prelude qualified

data Task x a
  = Done (Result x a)
  | Perform (IO (Task x a))

instance Functor (Task x) where
  fmap f (Done result) = Done (Result.map f result)
  fmap f (Perform io) = Perform (fmap (Task.map f) io)

instance Applicative (Task x) where
  pure = Done . Ok
  Done (Ok function) <*> task = Task.map function task
  Done (Error error) <*> _ = Done (Error error)
  Perform io <*> task = Perform (fmap (<*> task) io)

instance Monad (Task x) where
  Done (Ok value) >>= function = function value
  Done (Error error) >>= _ = Done (Error error)
  Perform io >>= function = Perform (fmap (>>= function) io)

instance MonadFail (Task String) where
  fail = Done . Error

evaluate :: Result x a -> Task x a
evaluate = Done

map :: (a -> b) -> Task x a -> Task x b
map = fmap

mapError :: (ErrorMessage y) => (x -> y) -> Task x a -> Task y a
mapError function (Done result) = Done (Result.mapError function result)
mapError function (Perform io) = Perform (fmap (mapError function) io)

fromIO :: IO a -> Task IOError a
fromIO io = Perform (Control.Exception.catch (fmap return io) (return . Done . Error))

toIO :: Task x a -> IO a
toIO (Done (Ok value)) = return value
toIO (Done (Error error)) = fail (errorMessage error)
toIO (Perform io) = io >>= toIO

main :: Task x () -> IO ()
main (Done (Ok ())) = System.Exit.exitSuccess
main (Done (Error error)) = System.Exit.die (errorMessage error)
main (Perform io) = io >>= main

forEach :: List a -> (a -> Task x ()) -> Task x ()
forEach values function = Prelude.mapM_ function values

collect :: (a -> Task x b) -> List a -> Task x (List b)
collect = Prelude.mapM
