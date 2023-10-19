module OpenSolidFFI () where

import OpenSolid hiding ((>>=))
import OpenSolidAPI (generateForeignFunctions)
import Prelude ((>>=))

$(generateForeignFunctions)
