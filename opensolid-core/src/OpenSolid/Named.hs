module OpenSolid.Named (Named (Named), fromLabel) where

import GHC.TypeLits (Symbol)

newtype Named (name :: Symbol) a = Named a

fromLabel :: forall name a. a -> Named name a
fromLabel = Named
