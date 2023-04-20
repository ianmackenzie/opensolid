module DoNotation
  ( Compose (compose)
  , Bind (bind)
  , Fail (fail)
  , (>>)
  , (>>=)
  )
where

import Basics
import Prelude qualified

class Compose a b c | a b -> c where
  compose :: a -> b -> c

instance b ~ b' => Compose (a -> b) (b' -> c) (a -> c) where
  compose f g = g Prelude.. f

class Bind p b where
  bind :: (a -> b) -> p a -> b

instance Bind Maybe (Maybe b) where
  bind f (Just value) = f value
  bind _ Nothing = Nothing

class Fail a where
  fail :: Text -> a

(>>) :: Compose a b c => a -> b -> c
(>>) = compose

(>>=) :: Bind p b => p a -> (a -> b) -> b
a >>= f = bind f a
