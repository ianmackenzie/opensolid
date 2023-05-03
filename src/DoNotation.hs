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

class Bind p q where
  bind :: (a -> q b) -> p a -> q b

instance Bind Maybe Maybe where
  bind f (Just value) = f value
  bind _ Nothing = Nothing

class Fail a where
  fail :: Text -> a

(>>) :: Compose a b c => a -> b -> c
(>>) = compose

(>>=) :: Bind p q => p a -> (a -> q b) -> q b
a >>= f = bind f a
