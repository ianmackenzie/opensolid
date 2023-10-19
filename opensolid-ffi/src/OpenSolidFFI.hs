module OpenSolidFFI () where

import OpenSolid
import OpenSolidAPI (generateForeignFunctions)

$(generateForeignFunctions)
