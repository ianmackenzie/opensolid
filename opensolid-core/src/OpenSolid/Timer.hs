module OpenSolid.Timer (Timer, start, elapsed) where

import GHC.Clock qualified
import OpenSolid.Duration (Duration)
import OpenSolid.Duration qualified as Duration
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity))

newtype Timer = Timer Number

getMonotonicTime :: IO Number
getMonotonicTime = IO.map Quantity GHC.Clock.getMonotonicTime

start :: IO Timer
start = IO.map Timer getMonotonicTime

elapsed :: Timer -> IO Duration
elapsed (Timer startTime) = do
  endTime <- getMonotonicTime
  return (Duration.seconds (endTime - startTime))
