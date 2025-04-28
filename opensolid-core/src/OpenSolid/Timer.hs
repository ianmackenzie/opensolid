module OpenSolid.Timer (Timer, start, elapsed) where

import GHC.Clock qualified
import OpenSolid.Duration (Duration)
import OpenSolid.Duration qualified as Duration
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Qty (Qty (Qty))

newtype Timer = Timer Float

getMonotonicTime :: IO Float
getMonotonicTime = IO.map Qty GHC.Clock.getMonotonicTime

start :: IO Timer
start = IO.map Timer getMonotonicTime

elapsed :: Timer -> IO Duration
elapsed (Timer startTime) = IO.do
  endTime <- getMonotonicTime
  IO.succeed (Duration.seconds (endTime - startTime))
