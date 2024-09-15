module Typeable (Typeable, equal) where

import Basics
import Data.Typeable (Typeable, (:~:) (Refl))
import Data.Typeable qualified

equal :: forall a b. (Eq a, Eq b, Typeable a, Typeable b) => a -> b -> Bool
equal a b = case Data.Typeable.eqT :: Maybe (a :~: b) of
  Just Data.Typeable.Refl -> a == b
  Nothing -> False
