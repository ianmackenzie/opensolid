module Duration
  ( Duration
  , zero
  , from
  , second
  , seconds
  , inSeconds
  , millisecond
  , milliseconds
  , inMilliseconds
  , microsecond
  , microseconds
  , inMicroseconds
  , minute
  , minutes
  , inMinutes
  , hour
  , hours
  , inHours
  )
where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified
import OpenSolid
import Qty qualified
import Units (Seconds)
import Units qualified
import Prelude qualified

type Duration = Qty Seconds

zero :: Duration
zero = Qty.zero

from :: UTCTime -> UTCTime -> Duration
from startTime endTime = do
  let diffTime = Data.Time.Clock.diffUTCTime endTime startTime
  seconds (Prelude.realToFrac diffTime)

second :: Duration
second = seconds 1.0

seconds :: Float -> Duration
seconds = Units.coerce

inSeconds :: Duration -> Float
inSeconds = Units.coerce

microsecond :: Duration
microsecond = seconds 1e-6

microseconds :: Float -> Duration
microseconds = (* microsecond)

inMicroseconds :: Duration -> Float
inMicroseconds = (/ microsecond)

millisecond :: Duration
millisecond = seconds 1e-3

milliseconds :: Float -> Duration
milliseconds = (* millisecond)

inMilliseconds :: Duration -> Float
inMilliseconds = (/ millisecond)

minute :: Duration
minute = seconds 60.0

minutes :: Float -> Duration
minutes = (* minute)

inMinutes :: Duration -> Float
inMinutes = (/ minute)

hour :: Duration
hour = minutes 60.0

hours :: Float -> Duration
hours = (* hour)

inHours :: Duration -> Float
inHours = (/ hour)
