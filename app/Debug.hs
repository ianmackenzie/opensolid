module Debug (toString) where

import OpenSolid
import qualified String

toString :: Show a => a -> String
toString value =
    String.fromList (show value)
