module OpenSolid.Result (Result (Success, Failure)) where

import {-# SOURCE #-} OpenSolid.Error qualified as Error

data Result x a where
  Success :: a -> Result x a
  Failure :: Error.Message x => x -> Result x a
