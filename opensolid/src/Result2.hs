module Result2
  ( Result2 (Succeed, Catch, Fail)
  , Error (message)
  , succeed
  , fail
  , catch
  , (>>=)
  , andThen
  , withDefault
  , map
  , map2
  , collect
  , combine
  )
where

import Arithmetic
import Basics
import Coalesce (Coalesce ((??)))
import Composition
import Data.Text qualified
import Data.Typeable (Typeable)
import Data.Typeable qualified
import Error ()
import GHC.Stack (CallStack, HasCallStack)
import GHC.Stack qualified
import System.IO.Error
import Type.Reflection qualified
import Prelude qualified

data Result2 a where
  Succeed :: a -> Result2 a
  Fail :: Error error => error -> CallStack -> Result2 a

deriving instance Show a => Show (Result2 a)

instance Prelude.Functor Result2 where
  fmap = map

instance Prelude.Applicative Result2 where
  pure = succeed

  Succeed function <*> Succeed value = Succeed (function value)
  Fail error callStack <*> _ = Fail error callStack
  Succeed _ <*> Fail error callStack = Fail error callStack

instance Prelude.Monad Result2 where
  (>>=) = (>>=)

instance Composition (Result2 ()) (Result2 a) (Result2 a) where
  Succeed () >> result = result
  Fail error callStack >> _ = Fail error callStack

instance a ~ b => Coalesce (Result2 a) (Result2 b) (Result2 a) where
  Succeed value ?? _ = Succeed value
  Fail _ _ ?? fallback = fallback

instance a ~ b => Coalesce (Result2 a) (Maybe b) (Maybe a) where
  Succeed value ?? _ = Just value
  Fail _ _ ?? fallback = fallback

instance a ~ b => Coalesce (Maybe a) (Result2 b) (Result2 a) where
  Just value ?? _ = Succeed value
  Nothing ?? fallback = fallback

instance a ~ b => Coalesce (Result2 a) (IO b) (IO a) where
  Succeed value ?? _ = Prelude.return value
  Fail _ _ ?? fallback = fallback

instance a ~ b => Coalesce (IO a) (Result2 b) (IO a) where
  io ?? fallback =
    System.IO.Error.catchIOError io $
      \_ -> case fallback of
        Succeed value -> Prelude.return value
        Fail error callStack -> Prelude.fail (Data.Text.unpack (output error callStack))

output :: Error error => error -> CallStack -> Text
output error callStack = do
  let prettyCallStack = Data.Text.pack (GHC.Stack.prettyCallStack callStack)
  "ERROR: " + errorType error + "\n" + message error + "\n" + prettyCallStack

errorType :: Error error => error -> Text
errorType error = do
  let typeConstructor = Type.Reflection.typeRepTyCon (Type.Reflection.typeOf error)
  let moduleName = Data.Text.pack (Type.Reflection.tyConModule typeConstructor)
  let typeConstructorName = Data.Text.pack (Type.Reflection.tyConName typeConstructor)
  moduleName + "." + typeConstructorName

class (Show error, Typeable error) => Error error where
  message :: error -> Text

instance Error (List Char) where
  message = Data.Text.pack

instance Error Text where
  message = identity

succeed :: a -> Result2 a
succeed = Succeed

fail :: (Error error, HasCallStack) => error -> Result2 a
fail error = Fail error GHC.Stack.callStack

catch :: Error error => (error -> Result2 a) -> Result2 a -> Result2 a
catch function result = case result of
  Succeed _ -> result
  Catch error -> function error
  Fail _ _ -> result

pattern Catch :: Error error => error -> Result2 a
pattern Catch error <- (extractError -> Just error)

extractError :: Error error => Result2 a -> Maybe error
extractError result = case result of
  Succeed _ -> Nothing
  Fail error _ -> Data.Typeable.cast error

(>>=) :: Result2 a -> (a -> Result2 b) -> Result2 b
Succeed value >>= function = function value
Fail error callStack >>= _ = Fail error callStack

andThen :: (a -> Result2 b) -> Result2 a -> Result2 b
andThen function result = result >>= function

withDefault :: a -> Result2 a -> a
withDefault fallback result = case result of
  Succeed value -> value
  Fail _ _ -> fallback

map :: (a -> b) -> Result2 a -> Result2 b
map function result = case result of
  Succeed value -> Succeed (function value)
  Fail error callStack -> Fail error callStack

map2 :: (a -> b -> value) -> Result2 a -> Result2 b -> Result2 value
map2 function result1 result2 = Result2.do
  value1 <- result1
  value2 <- result2
  succeed (function value1 value2)

collect :: (a -> Result2 b) -> List a -> Result2 (List b)
collect = Prelude.mapM

combine :: List (Result2 a) -> Result2 (List a)
combine = Prelude.sequence
