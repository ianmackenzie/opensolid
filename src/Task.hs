module Task
  ( Task
  , toIO
  , succeed
  , fail
  , map
  , mapError
  , forEach
  , perform
  )
where

import Basics
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

instance Bind [] (Task x (List b)) where
  [] >>= _ = succeed []
  (first : rest) >>= function =
    function first >>= (\firstResults -> map (firstResults ++) (rest >>= function))

map :: (a -> b) -> Task x a -> Task x b
map function (Done result) = Done (Result.map function result)
map function (Perform io) = Perform (Prelude.fmap (map function) io)

mapError :: IsError y => (x -> y) -> Task x a -> Task y a
mapError function (Done result) = Done (Result.mapError function result)
mapError function (Perform io) = Perform (Prelude.fmap (mapError function) io)

perform :: IO a -> Task IOError a
perform io =
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
