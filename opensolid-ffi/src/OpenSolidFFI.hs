module OpenSolidFFI () where

import OpenSolid hiding ((>>=))
import OpenSolidAPI (genreateForeignFunctions)
import Prelude ((>>=))

$(genreateForeignFunctions)
