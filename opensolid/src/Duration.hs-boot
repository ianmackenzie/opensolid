module Duration (Duration, inMicroseconds) where

import Float (Float)
import Qty (Qty)
import Units (Seconds)

type Duration = Qty Seconds

inMicroseconds :: Duration -> Float
