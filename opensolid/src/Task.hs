module Task
  ( Task
  , immediate
  , toIO
  , fail
  , map
  , mapError
  , each
  , fromIO
  , collect
  )
where

import Basics
import Control.Exception qualified
import Result (ErrorMessage (errorMessage), Result (Error, Ok))
import Result qualified
import System.Exit qualified

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

immediate :: Result x a -> Task x a
immediate = Done

map :: (a -> b) -> Task x a -> Task x b
map function (Done result) = Done (Result.map function result)
map function (Perform io) = Perform (fmap (map function) io)

mapError :: (ErrorMessage y) => (x -> y) -> Task x a -> Task y a
mapError function (Done result) = Done (Result.mapError function result)
mapError function (Perform io) = Perform (fmap (mapError function) io)

fromIO :: IO a -> Task IOError a
fromIO io = Perform (Control.Exception.catch (fmap return io) (return . Done . Error))

toIO :: Task x () -> IO ()
toIO (Done (Ok ())) = System.Exit.exitSuccess
toIO (Done (Error error)) = System.Exit.die (errorMessage error)
toIO (Perform io) = io >>= toIO

each :: (a -> Task x ()) -> List a -> Task x ()
each _ [] = return ()
each f (first : rest) = do f first; each f rest

collect :: (a -> Task x b) -> List a -> Task x (List b)
collect _ [] = return []
collect f (a : as) = do b <- f a; bs <- collect f as; return (b : bs)
