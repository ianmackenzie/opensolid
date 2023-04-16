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

instance Compose Bool (List a) (List a) where
  compose True list = list
  compose False _ = []

class Bind p b where
  bind :: (a -> b) -> p a -> b

instance Bind Maybe (Maybe b) where
  bind f (Just value) = f value
  bind _ Nothing = Nothing

instance Bind [] (List b) where
  bind f list = list Prelude.>>= f

instance Bind Maybe (List b) where
  bind f (Just value) = f value
  bind _ Nothing = []

class Fail a where
  fail :: Text -> a

(>>) :: Compose a b c => a -> b -> c
(>>) = compose

(>>=) :: Bind p b => p a -> (a -> b) -> b
a >>= f = bind f a
