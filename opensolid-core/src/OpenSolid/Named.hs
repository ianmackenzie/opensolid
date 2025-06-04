module OpenSolid.Named (Named (Named), fromLabel) where

import GHC.TypeLits (Symbol)
import OpenSolid.Composition

newtype Named (name :: Symbol) a = Named a

fromLabel :: forall name a. a -> Named name a
fromLabel = Named

instance
  Composition
    (Named name1 value1)
    (Named name2 value2)
    (Named name1 value1, Named name2 value2)
  where
  named1 >> named2 = (named1, named2)

instance
  Composition
    (Named name1 value1)
    (Named name2 value2, Named name3 value3)
    (Named name1 value1, Named name2 value2, Named name3 value3)
  where
  named1 >> (named2, named3) = (named1, named2, named3)

instance
  Composition
    (Named name1 value1)
    (Named name2 value2, Named name3 value3, Named name4 value4)
    (Named name1 value1, Named name2 value2, Named name3 value3, Named name4 value4)
  where
  named1 >> (named2, named3, named4) = (named1, named2, named3, named4)
