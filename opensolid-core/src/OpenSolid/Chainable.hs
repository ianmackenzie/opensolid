module OpenSolid.Chainable (Chainable (chain), empty) where

import OpenSolid.Prelude
import Prelude qualified

class Chainable action where
  empty :: action
  chain :: action -> action -> action

instance a1 ~ a2 => Chainable (a1 -> a2) where
  empty = id
  chain f g = \x -> let y = f x in seq y (g y)

instance Chainable (IO ()) where
  empty = Prelude.mempty
  chain = (>>)
