module OpenSolid.Curve1D.Zero (Zero (location, order, sign)) where

import OpenSolid.Prelude

data Zero = Zero
  { location :: Number
  , order :: Int
  , sign :: Sign
  }
