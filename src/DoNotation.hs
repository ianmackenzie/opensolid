module DoNotation
  ( Compose (compose)
  , Bind (bind)
  , Fail (fail)
  , (>>)
  , (<<)
  , (>>=)
  )
where

import Basics
import Prelude qualified

class Compose a b c | a b -> c where
  compose :: a -> b -> c

instance b ~ b' => Compose (a -> b) (b' -> c) (a -> c) where
  compose f g = g Prelude.. f

class Bind a b c where
  bind :: (b -> c) -> a -> c

instance a ~ a' => Bind (Maybe a) a' (Maybe b) where
  bind f (Just value) = f value
  bind _ Nothing = Nothing

class Fail a where
  fail :: Text -> a

(>>) :: Compose a b c => a -> b -> c
(>>) = compose

(<<) :: Compose a b c => b -> a -> c
(<<) = flip compose

(>>=) :: Bind a b c => a -> (b -> c) -> c
a >>= f = bind f a
