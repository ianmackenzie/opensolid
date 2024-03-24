module OpenSolidFFI () where

import OpenSolidAPI (generateForeignFunctions)
import Qty (Qty (Qty_)) -- Need to bring internal Qty_ constructor into scope so that Qty type is valid for FFI

$(generateForeignFunctions)
