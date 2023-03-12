module DoNotation
  ( Compose ((>>))
  , Bind ((>>=))
  , Fail (fail)
  )
where

import Basics
import Prelude qualified

class Compose a b c | a b -> c where
  (>>) :: a -> b -> c

instance b ~ b' => Compose (a -> b) (b' -> c) (a -> c) where
  f >> g = g Prelude.. f

instance Compose Bool (List a) (List a) where
  True >> list = list
  False >> _ = []

class Bind a b c | a b -> c where
  (>>=) :: a -> b -> c

instance a ~ a' => Bind (List a) (a' -> List b) (List b) where
  (>>=) = (Prelude.>>=)

instance a ~ a' => Bind (Maybe a) (a' -> List b) (List b) where
  Just value >>= function = function value
  Nothing >>= _ = []

class Fail a where
  fail :: Text -> a
